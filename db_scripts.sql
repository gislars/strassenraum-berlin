--https://trac.osgeo.org/postgis/ticket/2192
--TODO accept geography type AS parameter
CREATE OR REPLACE FUNCTION ST_Splap(geom1 geometry, geom2 geometry, double precision)
  RETURNS geometry AS 'SELECT ST_Split(ST_Snap($1, $2, $3), $2)'
  LANGUAGE sql IMMUTABLE STRICT COST 10;

--https://mygisnotes.wordpress.com/2017/01/01/split-lines-with-points-the-postgis-way/
CREATE OR REPLACE FUNCTION ST_AsMultiPoint(geometry) RETURNS geometry AS
'SELECT ST_Union((d).geom) FROM ST_DumpPoints($1) AS d;'
LANGUAGE sql IMMUTABLE STRICT COST 10;


--transform to local SRS , we can use meters instead of degree for calculations
--TODO check if all ST_* functions used are fine with geography type -> change to geography type
ALTER TABLE highways ADD COLUMN IF NOT EXISTS geog geography(LineString, 4326);
UPDATE highways SET geog = geom::geography;
ALTER TABLE highways ALTER COLUMN geom TYPE geometry(LineString, 25833) USING ST_Transform(geom, 25833);
ALTER TABLE highways ADD COLUMN IF NOT EXISTS angle numeric;
UPDATE highways SET angle = degrees(ST_Azimuth(ST_StartPoint(ST_Transform(geom, 25833)), ST_EndPoint(ST_Transform(geom, 25833))));
DROP INDEX IF EXISTS highways_geom_idx;
CREATE INDEX highways_geom_idx ON public.highways USING gist (geom);
DROP INDEX IF EXISTS highways_geog_idx;
CREATE INDEX highways_geog_idx ON public.highways USING gist (geog);

ALTER TABLE highways ADD COLUMN IF NOT EXISTS geog_buffer_left geography;
UPDATE highways SET geog_buffer_left = ST_Buffer(geog, 8, 'side=left');
ALTER TABLE highways ADD COLUMN IF NOT EXISTS geog_buffer_right geography;
UPDATE highways SET geog_buffer_right = ST_Buffer(geog, 8, 'side=right');

DROP INDEX IF EXISTS highways_geog_buffer_left_idx;
CREATE INDEX highways_geog_buffer_left_idx ON public.highways USING gist (geog_buffer_left);
DROP INDEX IF EXISTS highways_geog_buffer_right_idx;
CREATE INDEX highways_geog_buffer_right_idx ON public.highways USING gist (geog_buffer_right);


ALTER TABLE parking_poly ADD COLUMN IF NOT EXISTS geog geography;
UPDATE parking_poly SET geog = geom::geography;

DROP INDEX IF EXISTS parking_poly_geog_idx;
CREATE INDEX parking_poly_geog_idx ON public.parking_poly USING gist (geog);

ALTER TABLE service ADD COLUMN IF NOT EXISTS geog geography(LineString, 4326);
UPDATE service SET geog = ST_Transform(geom, 4326)::geography;
ALTER TABLE service ALTER COLUMN geom TYPE geometry(LineString, 25833) USING ST_Transform(geom, 25833);
ALTER TABLE service ADD COLUMN IF NOT EXISTS angle numeric;
UPDATE service SET angle = degrees(ST_Azimuth(ST_StartPoint(ST_Transform(geom, 25833)), ST_EndPoint(ST_Transform(geom, 25833))));
DROP INDEX IF EXISTS service_geom_idx;
CREATE INDEX service_geom_idx ON public.service USING gist (geom);
DROP INDEX IF EXISTS service_geog_idx;
CREATE INDEX service_geog_idx ON public.service USING gist (geog);


ALTER TABLE crossings ADD COLUMN IF NOT EXISTS geog geography(Point, 4326);
UPDATE crossings SET geog = geom::geography;
ALTER TABLE crossings ALTER COLUMN geom TYPE geometry(Point, 25833) USING ST_Transform(geom, 25833);
DROP INDEX IF EXISTS crossings_geom_idx;
CREATE INDEX crossings_geom_idx ON public.crossings USING gist (geom);
DROP INDEX IF EXISTS crossings_geog_idx;
CREATE INDEX crossings_geog_idx ON public.crossings USING gist (geog);

ALTER TABLE ramps ADD COLUMN IF NOT EXISTS geog geography(Point, 4326);
UPDATE ramps SET geog = geom::geography;
ALTER TABLE ramps ALTER COLUMN geom TYPE geometry(Point, 25833) USING ST_Transform(geom, 25833);
DROP INDEX IF EXISTS ramps_geom_idx;
CREATE INDEX ramps_geom_idx ON public.ramps USING gist (geom);
DROP INDEX IF EXISTS ramps_geog_idx;
CREATE INDEX ramps_geog_idx ON public.ramps USING gist (geog);


DROP SEQUENCE IF EXISTS highway_union_id;
CREATE SEQUENCE highway_union_id;

DROP TABLE IF EXISTS highway_union;
CREATE TABLE highway_union AS
WITH hw_union AS (
  SELECT
    row_number() over() id,
    name,
    (ST_LineMerge(ST_UNION(geog::geometry))) AS geom
  FROM highways
  GROUP BY name
)
SELECT
  nextval('highway_union_id') id,
  h.name highway_name,
  (ST_Dump(h.geom)).path part,
  (ST_Dump(h.geom)).geom geom,
  ((ST_Dump(h.geom)).geom)::geography geog
FROM
  hw_union h
;
ALTER TABLE highway_union ADD COLUMN IF NOT EXISTS geog_buffer_left geography;
UPDATE highway_union SET geog_buffer_left = ST_Buffer(geog, 8, 'side=left');
ALTER TABLE highway_union ADD COLUMN IF NOT EXISTS geog_buffer_right geography;
UPDATE highway_union SET geog_buffer_right = ST_Buffer(geog, 8, 'side=right');
DROP INDEX IF EXISTS highway_union_geog_idx;
CREATE INDEX highway_union_geog_idx ON highway_union USING gist (geog);
DROP INDEX IF EXISTS highway_union_geog_buffer_left_idx;
CREATE INDEX highway_union_geog_buffer_left_idx ON highway_union USING gist (geog_buffer_left);
DROP INDEX IF EXISTS highway_union_geog_buffer_right_idx;
CREATE INDEX highway_union_geog_buffer_right_idx ON highway_union USING gist (geog_buffer_right);


DROP TABLE IF EXISTS highway_crossings;
CREATE TABLE highway_crossings AS
SELECT
  row_number() over() id,
   count(DISTINCT h1.id) anzahl,
   ST_Intersection(h1.geog, h2.geog) geog,
   ST_Buffer(ST_Intersection(h1.geog, h2.geog), 5) geog_buffer,
   ST_Buffer(ST_Intersection(h1.geog, h2.geog), 10) geog_buffer10
FROM
  highway_union h1
  JOIN highway_union h2 ON ST_Intersects(h1.geog, h2.geog)
  and h1.id != h2.id
  and h1.highway_name IS DISTINCT FROM h2.highway_name
GROUP BY
   ST_Intersection(h1.geog, h2.geog)
;
DROP INDEX IF EXISTS highway_crossings_geog_buffer_idx;
CREATE INDEX highway_crossings_geog_buffer_idx ON highway_crossings USING gist (geog_buffer);
DROP INDEX IF EXISTS highway_crossings_geog_idx;
CREATE INDEX highway_crossings_geog_idx ON highway_crossings USING gist (geog);



DROP SEQUENCE IF EXISTS highway_union_parking_poly_id;
CREATE SEQUENCE highway_union_parking_poly_id;

DROP TABLE IF EXISTS highway_union_parking_poly;
CREATE TABLE highway_union_parking_poly AS
SELECT
  DISTINCT ON (h.id)
  nextval('highway_union_parking_poly_id') id,
  p.id p_id,
  h.id h_id,
  h.highway_name highway_name,
  h.geog_buffer_right,
  h.geog_buffer_left,
  ST_Transform(h.geog::geometry, 25833) geom,
  h.geog geog
FROM parking_poly p
JOIN LATERAL (
  SELECT
    h.*
  FROM
    highway_union h
  WHERE
    h.geog_buffer_right && p.geog
    OR h.geog_buffer_left && p.geog
  ORDER BY
    --order by biggest intersection area
    ST_Area(ST_Intersection(h.geog_buffer_right, p.geog)) + ST_Area(ST_Intersection(h.geog_buffer_left, p.geog)) DESC,
    --afterwards by smallest distance
    p.geog <-> h.geog
  LIMIT 1
) AS h ON true
WHERE
  (p.parking IN ('lane', 'street_side'))
  AND (p.access NOT IN ('private') OR p.access IS NULL)
;
DROP INDEX IF EXISTS highway_union_parking_poly_geom_idx;
CREATE INDEX highway_union_parking_poly_geom_idx ON highway_union_parking_poly USING gist (geom);
DROP INDEX IF EXISTS highway_union_parking_poly_geog_idx;
CREATE INDEX highway_union_parking_poly_geog_idx ON highway_union_parking_poly USING gist (geog);
DROP INDEX IF EXISTS highway_union_parking_poly_geog_buffer_right_idx;
CREATE INDEX highway_union_parking_poly_geog_buffer_right_idx ON highway_union_parking_poly USING gist (geog_buffer_right);
DROP INDEX IF EXISTS highway_union_parking_poly_geog_buffer_left_idx;
CREATE INDEX highway_union_parking_poly_geog_buffer_left_idx ON highway_union_parking_poly USING gist (geog_buffer_left);



DROP SEQUENCE IF EXISTS pp_points_id;
CREATE SEQUENCE pp_points_id;

DROP TABLE IF EXISTS pp_points;
CREATE TABLE pp_points AS

SELECT
  nextval('pp_points_id') id,
  'right' side,
  pp.id pp_id,
  pp.access "access",
  pp.capacity capacity,
  pp.parking parking,
  pp.building building,
  pp.parking_orientation parking_orientation,
  pp.parking_street_side_of parking_street_side_of,
  pp.parking_street_side_of_name parking_street_side_of_name,
  hs.highway_name highway_name,
  hs.id highway_id,
  ST_OrientedEnvelope(ST_Transform(pp.geom, 25833)) geom_envelope,
  ST_ConvexHull(pp.geom) geom_convex,
  ST_OrientedEnvelope(pp.geom) geom_concave,
  hs.geog highway_geog,
  pp.geog parking_geog,
  ((ST_DumpPoints(pp.geom)).geom)::geography <-> hs.geog distance,
  ((ST_DumpPoints(pp.geom)).path)[2] path,
  ((ST_DumpPoints(pp.geom)).geom)::geometry(Point, 4326),
  ((ST_DumpPoints(pp.geom)).geom)::geography geog,
  ST_Area(ST_Intersection(hs.geog_buffer_right, pp.geog)) intersection_aera
FROM
  parking_poly pp,
  highway_union_parking_poly hs
WHERE
  ST_Intersects(hs.geog_buffer_right, pp.geog)
  AND (pp.parking IN ('lane', 'street_side'))
  AND (pp.access NOT IN ('private') OR pp.access IS NULL)
UNION ALL
SELECT
  nextval('pp_points_id') id,
  'left' side,
  pp.id pp_id,
  pp.access "access",
  pp.capacity capacity,
  pp.parking parking,
  pp.building building,
  pp.parking_orientation parking_orientation,
  pp.parking_street_side_of parking_street_side_of,
  pp.parking_street_side_of_name parking_street_side_of_name,
  hs.highway_name highway_name,
  hs.id highway_id,
  ST_OrientedEnvelope(ST_Transform(pp.geom, 25833)) geom_envelope,
  ST_ConvexHull(pp.geom) geom_convex,
  ST_OrientedEnvelope(pp.geom) geom_concave,
  hs.geog highway_geog,
  pp.geog parking_geog,
  ((ST_DumpPoints(pp.geom)).geom)::geography <-> hs.geog distance,
  ((ST_DumpPoints(pp.geom)).path)[2] path,
  ((ST_DumpPoints(pp.geom)).geom)::geometry(Point, 4326),
  ((ST_DumpPoints(pp.geom)).geom)::geography geog,
  ST_Area(ST_Intersection(hs.geog_buffer_left, pp.geog)) intersection_aera
FROM
  parking_poly pp,
  highway_union_parking_poly hs
WHERE
  ST_Intersects(hs.geog_buffer_left, pp.geog)
  AND (pp.parking IN ('lane', 'street_side'))
  AND (pp.access NOT IN ('private') OR pp.access IS NULL)
;
DROP INDEX IF EXISTS pp_points_geom_idx;
CREATE INDEX pp_points_geom_idx ON pp_points USING gist (geom);
DROP INDEX IF EXISTS pp_points_geog_idx;
CREATE INDEX pp_points_geog_idx ON pp_points USING gist (geog);



DROP SEQUENCE IF EXISTS pl_separated_id;
CREATE SEQUENCE pl_separated_id;

--let all points of every parking_poly fall down on highway_segment
--sort them by position along the the segment
--so we can ST_MakeLine a new line along the highway_segment
DROP TABLE IF EXISTS pl_separated;
CREATE TABLE pl_separated AS
SELECT
  nextval('pl_separated_id') id,
  h.h_id h_id,
  p.side,
  p.pp_id,
  h.id highway_union_id,
  ARRAY_AGG(DISTINCT h.highway_name) highway_name,
  (MIN(p.distance) * -1) min_distance,
  ST_Transform(
    ST_MakeLine(
      ST_ClosestPoint(
        ST_Transform(h.geog::geometry, 3857),
        ST_Transform(p.geom, 3857)
      ) ORDER BY
        ST_LineLocatePoint(
          ST_Transform(h.geog::geometry, 3857),
          ST_ClosestPoint(ST_Transform(h.geog::geometry, 3857), ST_Transform(p.geom, 3857))
        )
    ),
    4326
  )::geometry(Linestring, 4326) geom
FROM
  pp_points p
  JOIN LATERAL (
    SELECT
      h.*
    FROM
      highway_union_parking_poly h
    WHERE
      h.geog_buffer_right && p.geog
    ORDER BY
      --order by biggest intersection area
      ST_Area(ST_Intersection(h.geog_buffer_right, p.geog)) DESC,
      --afterwards by smallest distance
      p.geog <-> h.geog
    LIMIT 1
  ) AS h ON true
WHERE
  p.side = 'right'
  AND p.highway_name = h.highway_name
GROUP BY
  p.pp_id,
  p.side,
  h.h_id,
  h.id
UNION ALL
SELECT
  nextval('pl_separated_id') id,
  h.h_id h_id,
  p.side,
  p.pp_id,
  h.id highway_union_id,
  ARRAY_AGG(DISTINCT h.highway_name) highway_name,
  MIN(p.distance) min_distance,
  ST_Transform(
    ST_MakeLine(
      ST_ClosestPoint(
        ST_Transform(h.geog::geometry, 3857),
        ST_Transform(p.geom, 3857)
      ) ORDER BY
        ST_LineLocatePoint(
          ST_Transform(h.geog::geometry, 3857),
          ST_ClosestPoint(ST_Transform(h.geog::geometry, 3857), ST_Transform(p.geom, 3857))
        )
    ),
    4326
  )::geometry(Linestring, 4326) geom
FROM
  pp_points p
  JOIN LATERAL (
    SELECT
      h.*
    FROM
      highway_union_parking_poly h
    WHERE
      h.geog_buffer_left && p.geog
    ORDER BY
      --order by biggest intersection area
      ST_Area(ST_Intersection(h.geog_buffer_left, p.geog)) DESC,
      --afterwards by smallest distance
      p.geog <-> h.geog
    LIMIT 1
  ) AS h ON true
WHERE
  p.side = 'left'
  AND p.highway_name = h.highway_name
GROUP BY
  p.pp_id,
  p.side,
  h.h_id,
  h.id
;
ALTER TABLE pl_separated ADD COLUMN IF NOT EXISTS geog geography(LineString, 4326);
UPDATE pl_separated SET geog = geom::geography;
DROP INDEX IF EXISTS pl_separated_geom_idx;
CREATE INDEX pl_separated_geom_idx ON pl_separated USING gist (geom);
DROP INDEX IF EXISTS pl_separated_geog_idx;
CREATE INDEX pl_separated_geog_idx ON pl_separated USING gist (geog);




DROP TABLE IF EXISTS pl_separated_union;
CREATE TABLE pl_separated_union AS
SELECT
  row_number() over() id,
  p.h_id,
  p.side,
  MIN(p.min_distance) min_distance,
  (ST_LineMerge(ST_Union(p.geom)))::geography geog
FROM
  pl_separated p
GROUP BY
  p.side, p.h_id
;
DROP INDEX IF EXISTS pl_separated_union_geog_idx;
CREATE INDEX pl_separated_union_geog_idx ON pl_separated_union USING gist (geog);



DROP TABLE IF EXISTS parking_lanes;
CREATE TABLE parking_lanes AS
SELECT
  row_number() over() id,
  a.way_id,
  v.side,
  a.type highway,
  a.name "highway:name",
  a.parking_lane_width_proc "highway:width_proc",
  a.parking_lane_width_effective "highway:width_proc:effective",
  a.surface,
  a.parking,
  CASE WHEN v.side = 'left' THEN a.parking_lane_left
       WHEN v.side = 'right' THEN a.parking_lane_right
  END "orientation",
  CASE WHEN v.side = 'left' THEN a.parking_lane_left_position
       WHEN v.side = 'right' THEN a.parking_lane_right_position
  END "position",
  CASE WHEN v.side = 'left' THEN a.parking_condition_left
       WHEN v.side = 'right' THEN a.parking_condition_right
  END "condition",
  CASE WHEN v.side = 'left' THEN a.parking_condition_left_other
       WHEN v.side = 'right' THEN a.parking_condition_right_other
  END "condition:other",
  CASE WHEN v.side = 'left' THEN a.parking_condition_left_other_time
       WHEN v.side = 'right' THEN a.parking_condition_right_other_time
  END "condition:other:time",
  CASE WHEN v.side = 'left' THEN a.parking_condition_left_maxstay
       WHEN v.side = 'right' THEN a.parking_condition_right_maxstay
  END maxstay,
  CASE
    WHEN v.side = 'left' AND a.parking_lane_left_capacity IS NOT NULL THEN a.parking_lane_left_capacity
    WHEN v.side = 'right' AND a.parking_lane_right_capacity IS NOT NULL THEN a.parking_lane_right_capacity
    ELSE NULL
  END capacity_osm,
  CASE
    WHEN v.side = 'left' AND a.parking_lane_left_capacity IS NOT NULL THEN a.parking_lane_left_source_capacity
    WHEN v.side = 'right' AND a.parking_lane_right_capacity IS NOT NULL THEN a.parking_lane_right_source_capacity
    ELSE NULL
  END "source:capacity_osm",
  0 capacity,
  'estimated' "source:capacity",
  CASE WHEN v.side = 'left' THEN a.parking_lane_left_width_carriageway
       WHEN v.side = 'right' THEN a.parking_lane_right_width_carriageway
  END width,
  CASE WHEN v.side = 'left' THEN a.parking_lane_left_offset
       WHEN v.side = 'right' THEN a.parking_lane_right_offset
  END "offset",
  CASE
    -- before offsetting we cut out all separated parking lanes
    WHEN v.side IN ('left') THEN 
      ST_Transform(
        ST_OffsetCurve(
          ST_Transform(
            st_difference(
              a.geog::geometry,
              ST_SetSRID(COALESCE(ST_Buffer(s.geog, 0.2), 'GEOMETRYCOLLECTION EMPTY'::geography), 4326)::geometry
            ),
            25833
          ), 
          COALESCE(s.min_distance, a.parking_lane_left_offset)
        ), 4326
      )::geography
    WHEN v.side IN ('right') THEN 
      ST_Transform(
        ST_OffsetCurve(
          ST_Transform(
            st_difference(
              a.geog::geometry,
              ST_SetSRID(COALESCE(ST_Buffer(s.geog, 0.2), 'GEOMETRYCOLLECTION EMPTY'::geography), 4326)::geometry
            ),
            25833
          ), 
          COALESCE(s.min_distance, a.parking_lane_right_offset)
        ), 4326
      )::geography
  END geog,
  a.error_output
FROM
  (VALUES ('left'), ('right')) AS v(side)
  CROSS JOIN
  highways a
  LEFT JOIN pl_separated_union s ON ST_Intersects(s.geog, ST_Buffer(a.geog, 0.2)) AND v.side = s.side
UNION ALL
SELECT
  row_number() over() id,
  NULL way_id,
  pl.side,
  NULL highway,
  pl.highway_name::text "highway:name",
  NULL "highway:width_proc",
  NULL "highway:width_proc:effective",
  NULL surface,
  p.parking,
  p.parking_orientation "orientation",
  NULL "position",
  NULL "condition",
  NULL "condition:other",
  NULL "condition:other:time",
  NULL maxstay,
  p.capacity capacity_osm,
  'OSM' "source:capacity_osm",
  p.capacity capacity,
  'OSM' "source:capacity",
  NULL width,
  pl.min_distance "offset",
  ST_Transform(ST_OffsetCurve(ST_Transform(pl.geog::geometry, 25833), pl.min_distance), 4326)::geography geog,
  NULL error_output
FROM
  pl_separated pl
  LEFT JOIN parking_poly p ON p.id = pl.pp_id
;
DROP INDEX IF EXISTS parking_lanes_geog_idx;
CREATE INDEX parking_lanes_geog_idx ON parking_lanes USING gist (geog);


DROP TABLE IF EXISTS ped_crossings;
CREATE TABLE ped_crossings AS
SELECT
  row_number() over() id,
  c.node_id,
  p.side,
  h.way_id highway_id,
  c.highway,
  c.crossing,
  c.crossing_ref,
  c.kerb,
  c.crossing_buffer_marking "crossing:buffer_marking",
  c.crossing_kerb_extension "crossing:kerb_extension",
  c.traffic_signals_direction "traffic_signals:direction",
  h.parking_lane_width_proc "width_proc",
  CASE
    WHEN p.side IN ('left') THEN h.parking_lane_left_width_carriageway
    WHEN p.side IN ('right') THEN h.parking_lane_right_width_carriageway
  END "parking:lane:width:carriageway",
  h.parking_lane_left_width_carriageway "parking:lane:left:width:carriageway",
  h.parking_lane_right_width_carriageway "parking:lane:right:width:carriageway",
  c.geom geom,
  CASE
    WHEN p.side IN ('left') THEN ST_Transform(ST_ClosestPoint(ST_Transform(p.geog::geometry, 3857), ST_Transform(c.geog::geometry, 3857)), 4326)::geography
    WHEN p.side IN ('right') THEN ST_Transform(ST_ClosestPoint(ST_Transform(p.geog::geometry, 3857), ST_Transform(c.geog::geometry, 3857)), 4326)::geography
  END geog_offset,
  CASE
     WHEN p.side IN ('left') THEN
       CASE
        WHEN c.highway = 'traffic_signals' AND c.traffic_signals_direction IN ('forward', 'backward') THEN ST_Buffer(ST_Transform(ST_ClosestPoint(ST_Transform(p.geog::geometry, 3857), ST_Transform(c.geog::geometry, 3857)), 4326)::geography, 10)
        WHEN c.crossing_kerb_extension = 'both' OR c.crossing_buffer_marking = 'both' THEN ST_Buffer(ST_Transform(ST_ClosestPoint(ST_Transform(p.geog::geometry, 3857), ST_Transform(c.geog::geometry, 3857)), 4326)::geography, 3)
        WHEN c.crossing = 'zebra' OR c.crossing_ref = 'zebra' OR c.crossing = 'traffic_signals' THEN ST_Buffer(ST_Transform(ST_ClosestPoint(ST_Transform(p.geog::geometry, 3857), ST_Transform(c.geog::geometry, 3857)), 4326)::geography, 4.5)
        WHEN c.crossing = 'marked' THEN ST_Buffer(ST_Transform(ST_ClosestPoint(ST_Transform(p.geog::geometry, 3857), ST_Transform(c.geog::geometry, 3857)), 4326)::geography, 2)
      END
    WHEN p.side IN ('right') THEN
      CASE
        WHEN c.highway = 'traffic_signals' AND c.traffic_signals_direction IN ('forward', 'backward') THEN ST_Buffer(ST_Transform(ST_ClosestPoint(ST_Transform(p.geog::geometry, 3857), ST_Transform(c.geog::geometry, 3857)), 4326)::geography, 10)
        WHEN c.crossing_kerb_extension = 'both' OR c.crossing_buffer_marking = 'both' THEN ST_Buffer(ST_Transform(ST_ClosestPoint(ST_Transform(p.geog::geometry, 3857), ST_Transform(c.geog::geometry, 3857)), 4326)::geography, 3)
        WHEN c.crossing = 'zebra' OR c.crossing_ref = 'zebra' OR c.crossing = 'traffic_signals' THEN ST_Buffer(ST_Transform(ST_ClosestPoint(ST_Transform(p.geog::geometry, 3857), ST_Transform(c.geog::geometry, 3857)), 4326)::geography, 4.5)
        WHEN c.crossing = 'marked' THEN ST_Buffer(ST_Transform(ST_ClosestPoint(ST_Transform(p.geog::geometry, 3857), ST_Transform(c.geog::geometry, 3857)), 4326)::geography, 2)
      END
  END geog_offset_buffer
FROM
  (VALUES ('left'), ('right')) _(side)
  CROSS JOIN
  crossings c
  JOIN highways h ON ST_Intersects(c.geog, h.geog)
  JOIN parking_lanes p ON p.way_id = h.way_id
WHERE
  "crossing_buffer_marking" IS NOT NULL OR
  "crossing_kerb_extension" IS NOT NULL OR
  c.highway = 'traffic_signals' OR
  c.crossing != 'unmarked'
;

DROP TABLE IF EXISTS crossing_buffer;
CREATE TABLE crossing_buffer AS
SELECT
  row_number() over() id,
  highway,
  crossing,
  crossing_ref,
  kerb,
  "crossing:buffer_marking",
  "crossing:kerb_extension",
  "traffic_signals:direction",
  "width_proc",
  "parking:lane:left:width:carriageway",
  "parking:lane:right:width:carriageway",
  CASE
    WHEN highway = 'traffic_signals' AND "traffic_signals:direction" IN ('forward', 'backward') THEN ST_Buffer(geog_offset_buffer, 10)
    WHEN "crossing:kerb_extension" = 'both' OR "crossing:buffer_marking" = 'both' THEN ST_Buffer(geog_offset_buffer, 3)
    WHEN crossing = 'zebra' OR crossing_ref = 'zebra' OR crossing = 'traffic_signals' THEN ST_Buffer(geog_offset_buffer, 4.5)
    WHEN crossing = 'marked' THEN ST_Buffer(geog_offset_buffer, 2)
    --ELSE ST_Buffer(geom, 20)
  END geog
FROM
  ped_crossings
;



DROP TABLE IF EXISTS ssr;
CREATE TABLE ssr AS
SELECT
  row_number() over() id,
  s.type,
  s.surface,
  s.name,
  s.parking,
  s.parking_lane_left,
  s.parking_lane_right,
  s.parking_lane_width_proc,
  s.parking_lane_width_effective,
  s.parking_lane_left_position,
  s.parking_lane_right_position,
  s.parking_lane_left_width,
  s.parking_lane_right_width,
  s.parking_lane_left_width_carriageway,
  s.parking_lane_right_width_carriageway,
  s.parking_lane_left_offset,
  s.parking_lane_right_offset,
  s.error_output,
  ST_Buffer(ST_Intersection(s.geog, h.geog), (h.parking_lane_width_proc / 2) + 5) geog
FROM service s
  JOIN highways h ON ST_Intersects(s.geog, h.geog)
WHERE
 s.parking_lane_left IS NOT NULL
 OR s.parking_lane_right IS NOT NULL
;

DROP TABLE IF EXISTS driveways;
CREATE TABLE driveways AS
SELECT
  row_number() over() id,
  s.type,
  s.surface,
  s.name,
  s.parking,
  s.parking_lane_left,
  s.parking_lane_right,
  s.parking_lane_width_proc,
  s.parking_lane_width_effective,
  s.parking_lane_left_position,
  s.parking_lane_right_position,
  s.parking_lane_left_width,
  s.parking_lane_right_width,
  s.parking_lane_left_width_carriageway,
  s.parking_lane_right_width_carriageway,
  s.parking_lane_left_offset,
  s.parking_lane_right_offset,
  s.error_output,
  ST_Buffer(ST_Intersection(s.geog, p.geog), GREATEST((s.parking_lane_width_proc / 2), 2) ) geog
FROM service s
  JOIN parking_lanes p ON ST_Intersects(s.geog, p.geog)
;

DROP TABLE IF EXISTS kerb_intersection_points;
CREATE TABLE kerb_intersection_points AS
SELECT
  a.id,
  a.side,
  a."highway" AS "type",
  a."highway:name" AS "name",
  a.orientation parking_lane,
  a.position parking_lane_position,
  a.width parking_lane_width,
  a.offset parking_lane_offset,
  CASE
    WHEN (a.orientation NOT IN ('no','no_stopping','no_parking') AND b.orientation IN ('no','no_stopping','no_parking'))
      OR (a.orientation IN ('no','no_stopping','no_parking') AND b.orientation NOT IN ('no','no_stopping','no_parking')) THEN 'no_stop'
    WHEN a.highway IS NOT DISTINCT FROM b.highway
      AND a."highway:name" IS NOT DISTINCT FROM b."highway:name"
      AND a.side IS NOT DISTINCT FROM b.side
      AND a.orientation IS NOT DISTINCT FROM b.orientation
      AND a.parking IS NOT DISTINCT FROM b.parking THEN 'same_street'
    WHEN a."highway" IN ('pedestrian')
      OR b."highway" IN ('pedestrian') THEN 'pedestrian'
    ELSE 'fine'
  END crossing_debug,
  ST_CollectionExtract(ST_Intersection(a.geog::geometry, b.geog::geometry), 1)::geography geog,
  ST_Buffer(ST_CollectionExtract(ST_Intersection(a.geog::geometry, b.geog::geometry), 1)::geography, 5) geog_buff
FROM
  parking_lanes a,
  parking_lanes b
WHERE
  ST_Intersects(a.geog, b.geog)
  AND a.id != b.id
;

DROP TABLE IF EXISTS pl_dev;
CREATE TABLE pl_dev AS
WITH driveways AS (
  SELECT
    p.id,
    (ST_Union(d.geog::geometry))::geography geog
  FROM
    parking_lanes p JOIN driveways d ON st_intersects(d.geog, p.geog)
  GROUP BY
    p.id
), pedestrian_crossings AS (
  SELECT
    p.id,
    (ST_Union(c.geog_offset_buffer::geometry))::geography geog
  FROM
    ped_crossings c JOIN parking_lanes p ON st_intersects(p.geog, c.geog_offset_buffer)
  GROUP BY
    p.id
), kerb_intersections AS (
  SELECT
    p.id,
    (ST_Union(k.geog_buff::geometry))::geography geog
  FROM
    kerb_intersection_points k JOIN parking_lanes p ON st_intersects(p.geog, k.geog_buff)
  WHERE
    crossing_debug NOT IN ('same_street')
  GROUP BY
   p.id
), highways_buffer AS (
  SELECT
    p.id,
    (ST_Union(ST_Buffer(h.geog, 3)::geometry))::geography geog
  FROM
    highways h JOIN parking_lanes p ON st_intersects(p.geog, st_buffer(h.geog, 2))
  GROUP BY
    p.id
)
SELECT
  DISTINCT p.id id,
  p.way_id way_id,
  p.side side,
  p.highway highway,
  p."highway:name" "highway:name",
  p."highway:width_proc" "highway:width_proc",
  p."highway:width_proc:effective" "highway:width_proc:effective",
  p.surface surface,
  p.parking parking,
  p.orientation orientation,
  p."position" "position",
  p.condition condition,
  p."condition:other" "condition:other",
  p."condition:other:time" "condition:other:time",
  p.maxstay maxstay,
  p.capacity_osm capacity_osm,
  p."source:capacity_osm" "source:capacity_osm",
  p.capacity capacity,
  p."source:capacity" "source:capacity",
  p.width width,
  p."offset" "offset",
  p.geog geog,
  p.error_output error_output,
  st_difference(
    st_difference(
      st_difference(
        st_difference(
          p.geog::geometry,
          ST_SetSRID(COALESCE(d.geog, 'GEOMETRYCOLLECTION EMPTY'::geography), 4326)::geometry
        ),
        ST_SetSRID(COALESCE(c.geog, 'GEOMETRYCOLLECTION EMPTY'::geography), 4326)::geometry
      ),
      ST_SetSRID(COALESCE(k.geog, 'GEOMETRYCOLLECTION EMPTY'::geography), 4326)::geometry
    ),
    ST_SetSRID(COALESCE(hb.geog, 'GEOMETRYCOLLECTION EMPTY'::geography), 4326)::geometry
  )::geography geog_diff,
  d.geog driveway_geog,
  c.geog ped_crossing_geog,
  k.geog kerbs_geog,
  hb.geog highways_buffer_geog
FROM
  parking_lanes p
  LEFT JOIN driveways d ON p.id = d.id
  LEFT JOIN pedestrian_crossings c ON p.id = c.id
  LEFT JOIN kerb_intersections k ON p.id = k.id
  LEFT JOIN highways_buffer hb ON p.id = hb.id
;

DROP TABLE IF EXISTS pl_dev_geog;
CREATE TABLE pl_dev_geog AS
WITH defval AS (
  SELECT
    5.2 vehicle_dist_para,
    3.1 vehicle_dist_diag,
    2.5 vehicle_dist_perp,
    4.4 vehicle_length,
    1.8 vehicle_width
), dv AS (
  SELECT
    *,
    sqrt(d.vehicle_width * 0.5 * d.vehicle_width) + sqrt(d.vehicle_length * 0.5 * d.vehicle_length) vehicle_diag_width
  FROM defval d
), single_geog AS (
SELECT
    h.*,
    (ST_DUMP(h.geog_diff::geometry)).path,
    ((ST_DUMP(h.geog_diff::geometry)).geom)::geography simple_geog
FROM
  pl_dev h
)
SELECT
    COALESCE((single.way_id::text  || '.' || single.path[1]::text), single.way_id::text) way_id,
    row_number() over() id,
    single.side side ,
    single.highway highway ,
    single."highway:name" "highway:name",
    single."highway:width_proc" "highway:width_proc",
    single."highway:width_proc:effective" "highway:width_proc:effective",
    single.surface surface,
    single.parking parking,
    single.orientation orientation,
    single."position" "position",
    single.condition condition,
    single."condition:other" "condition:other",
    single."condition:other:time" "condition:other:time",
    single.maxstay maxstay,
    single.capacity_osm capacity_osm,
    single."source:capacity_osm" "source:capacity_osm",
    CASE
      --WHEN side = 'left' AND a.parking_lane_left_capacity IS NOT NULL THEN a.parking_lane_left_capacity
      WHEN side = 'left' THEN
        CASE
          WHEN single.orientation = 'parallel' AND ST_Length(single.simple_geog) > dv.vehicle_length THEN floor((ST_Length(single.simple_geog) + (dv.vehicle_dist_para - dv.vehicle_length)) / dv.vehicle_dist_para)
          WHEN single.orientation = 'diagonal' AND ST_Length(single.simple_geog) > dv.vehicle_diag_width THEN floor((ST_Length(single.simple_geog) + (dv.vehicle_dist_diag - dv.vehicle_diag_width)) / dv.vehicle_dist_diag)
          WHEN single.orientation = 'perpendicular' AND ST_Length(single.simple_geog) > dv.vehicle_width THEN floor((ST_Length(single.simple_geog) + (dv.vehicle_dist_perp - dv.vehicle_width)) / dv.vehicle_dist_perp)
        END
      --WHEN side = 'right' AND a.parking_lane_right_capacity IS NOT NULL THEN a.parking_lane_right_capacity
      WHEN side = 'right' THEN
        CASE
          WHEN single.orientation = 'parallel' AND ST_Length(single.simple_geog) > dv.vehicle_length THEN floor((ST_Length(single.simple_geog) + (dv.vehicle_dist_para - dv.vehicle_length)) / dv.vehicle_dist_para)
          WHEN single.orientation = 'diagonal' AND ST_Length(single.simple_geog) > dv.vehicle_diag_width THEN floor((ST_Length(single.simple_geog) + (dv.vehicle_dist_diag - dv.vehicle_diag_width)) / dv.vehicle_dist_diag)
          WHEN single.orientation = 'perpendicular' AND ST_Length(single.simple_geog) > dv.vehicle_width THEN floor((ST_Length(single.simple_geog) + (dv.vehicle_dist_perp - dv.vehicle_width)) / dv.vehicle_dist_perp)
        END
    END capacity,
    'estimated' "source:capacity",
    single.width width,
    single."offset" "offset",
    single.geog single_geog,
    single.error_output error_output,
    single.geog_diff geog_diff,
    (single.simple_geog)::geography geog
FROM
  single_geog single,
  dv
;





DROP TABLE IF EXISTS parking_segments;
CREATE TABLE parking_segments AS
SELECT

    way_id way_id,
    id id,
    side side,
    highway highway,
    "highway:name" highway_name,
    "highway:width_proc" highway_width_proc,
    "highway:width_proc:effective" highway_width_proc_effective,
    surface surface,
    parking parking,
    orientation orientation,
    "position" "position",
    condition condition,
    "condition:other" condition_other,
    "condition:other:time" condition_other_time,
    maxstay maxstay,
    capacity_osm capacity_osm,
    "source:capacity_osm" source_capacity_osm,
    capacity capacity,
    "source:capacity" source_capacity,
    width width,
    "offset" "offset",
    error_output error_output,
    geog::geometry(LineString, 4326) geom
FROM pl_dev_geog
WHERE ST_Length(geog) > 4
;
DROP INDEX IF EXISTS parking_segments_geom_idx;
CREATE INDEX parking_segments_geom_idx ON parking_segments USING gist (geom);

