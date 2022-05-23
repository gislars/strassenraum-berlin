--https://trac.osgeo.org/postgis/ticket/2192
--TODO accept geography type as parameter
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



DROP TABLE IF EXISTS highway_union;
CREATE TABLE highway_union AS
SELECT
  row_number() over() id,
  name,
  (ST_LineMerge(ST_UNION(geog::geometry)))::geography as geog
FROM highways
GROUP BY name
;
DROP INDEX IF EXISTS highway_union_geog_idx;
CREATE INDEX highway_union_geog_idx ON highway_union USING gist(geog);



DROP TABLE IF EXISTS highway_crossings;
CREATE TABLE highway_crossings AS
SELECT
  row_number() over() id,
   count(DISTINCT h1.id) anzahl,
   ST_Intersection(h1.geog, h2.geog) geog,
   ST_Buffer(ST_Intersection(h1.geog, h2.geog), 5) geog_buffer,
   ST_Buffer(ST_Intersection(h1.geog, h2.geog), 10) geog_buffer10
FROM
  highway_segments h1
  JOIN highway_segments h2 ON ST_Intersects(h1.geog, h2.geog)
  and h1.id != h2.id
  and h1.line_name IS DISTINCT FROM h2.line_name
GROUP BY
   ST_Intersection(h1.geog, h2.geog)
;
DROP INDEX IF EXISTS highway_crossings_geog_buffer_idx;
CREATE INDEX highway_crossings_geog_buffer_idx ON highway_crossings USING gist (geog_buffer);
DROP INDEX IF EXISTS highway_crossings_geog_idx;
CREATE INDEX highway_crossings_geog_idx ON highway_crossings USING gist (geog);



DROP SEQUENCE IF EXISTS highway_segments_id;
CREATE SEQUENCE highway_segments_id;

DROP TABLE IF EXISTS highway_segments;
CREATE TABLE highway_segments as
WITH crossing_intersecting_highways AS(
   SELECT
     h.id AS lines_id,
     h.name as line_name,
     h.geog AS line_geog,
     (ST_Union(c.geog::geometry))::geography AS blade
   FROM highway_union h, highway_crossings c
    WHERE h.geog && c.geog_buffer
   GROUP BY h.id, h.name, h.geog
)
SELECT
  nextval('highway_segments_id') id,
  lines_id,
  line_name,
  -- TODO let ST_Splap accept geography
  ((ST_Dump(ST_Splap(line_geog::geometry, blade::geometry, 0.0000000000001))).geom)::geography geog
FROM
  crossing_intersecting_highways
;

DROP INDEX IF EXISTS highway_segments_geog_idx;
CREATE INDEX highway_segments_geog_idx ON highway_segments USING gist(geog);



DROP SEQUENCE IF EXISTS crossing_cake_id;
CREATE SEQUENCE crossing_cake_id;

DROP TABLE IF EXISTS crossing_cake;
CREATE TABLE crossing_cake as
WITH highway_segements_crossing AS(
   SELECT
     c.id AS crossing_id,
     c.anzahl anzahl,
     c.geog_buffer AS c_buffer,
     (ST_Union(h.geog::geometry))::geography AS blade
   FROM highway_segments h, highway_crossings c
    WHERE h.geog && c.geog_buffer
   GROUP BY c.id, c.anzahl, c.geog_buffer
)
SELECT
  nextval('crossing_cake_id') id,
  crossing_id,
  anzahl,
  0 c_area,
  ((ST_Dump(ST_Split(c_buffer::geometry, blade::geometry))).geom)::geography geog
FROM
  highway_segements_crossing
;

UPDATE crossing_cake SET c_area = ST_Area(geog);




DROP TABLE IF EXISTS parking_lanes;
CREATE TABLE parking_lanes AS
WITH defval AS (
  SELECT
    5.2 vehicle_dist_para,
    3.1 vehicle_dist_diag,
    2.5 vehicle_dist_perp,
    4.4 vehicle_length,
    1.8 vehicle_width
), dv as (
  SELECT
    *,
    sqrt(d.vehicle_width * 0.5 * d.vehicle_width) + sqrt(d.vehicle_length * 0.5 * d.vehicle_length) vehicle_diag_width
  FROM defval d
)
  SELECT
    row_number() over() id,
    a.way_id,
    side,
    a.type highway,
    a.name "highway:name",
    a.parking_lane_width_proc "highway:width_proc",
    a.parking_lane_width_effective "highway:width_proc:effective",
    a.surface,
    a.parking,
    CASE WHEN side = 'left' THEN a.parking_lane_left
         WHEN side = 'right' THEN a.parking_lane_right
    END "orientation",
    CASE WHEN side = 'left' THEN a.parking_lane_left_position
         WHEN side = 'right' THEN a.parking_lane_right_position
    END "position",
    CASE WHEN side = 'left' THEN a.parking_condition_left
         WHEN side = 'right' THEN a.parking_condition_right
    END "condition",
    CASE WHEN side = 'left' THEN a.parking_condition_left_other
         WHEN side = 'right' THEN a.parking_condition_right_other
    END "condition:other",
    CASE WHEN side = 'left' THEN a.parking_condition_left_other_time
         WHEN side = 'right' THEN a.parking_condition_right_other_time
    END "condition:other:time",
    CASE WHEN side = 'left' THEN a.parking_condition_left_maxstay
         WHEN side = 'right' THEN a.parking_condition_right_maxstay
    END maxstay,
    CASE
      WHEN side = 'left' AND a.parking_lane_left_capacity IS NOT NULL THEN a.parking_lane_left_capacity
      WHEN side = 'right' AND a.parking_lane_right_capacity IS NOT NULL THEN a.parking_lane_right_capacity
      ELSE NULL
    END capacity_osm,
    CASE
      WHEN side = 'left' AND a.parking_lane_left_capacity IS NOT NULL THEN a.parking_lane_left_source_capacity
      WHEN side = 'right' AND a.parking_lane_right_capacity IS NOT NULL THEN a.parking_lane_right_source_capacity
      ELSE NULL
    END "source:capacity_osm",
    0 capacity,
    'estimated' "source:capacity",
    CASE WHEN side = 'left' THEN a.parking_lane_left_width_carriageway
         WHEN side = 'right' THEN a.parking_lane_right_width_carriageway
    END width,
    CASE WHEN side = 'left' THEN a.parking_lane_left_offset
         WHEN side = 'right' THEN a.parking_lane_right_offset
    END "offset",
    CASE
      -- we have to st_transform into an metric srs since the offset is using the unit of the srs (4326 uses deegrees)
      WHEN side IN ('left') THEN ST_Transform(ST_OffsetCurve(ST_Transform(a.geog::geometry, 25833), a.parking_lane_left_offset), 4326)::geography
      WHEN side IN ('right') THEN ST_Transform(ST_OffsetCurve(ST_Transform(a.geog::geometry, 25833), a.parking_lane_right_offset), 4326)::geography
    END geog,
    a.error_output
  FROM
    (VALUES ('left'), ('right')) _(side)
    CROSS JOIN
    highways a,
    dv
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
    WHEN p.side IN ('left') THEN ST_ClosestPoint(p.geog::geometry, c.geog::geometry)::geography
    WHEN p.side IN ('right') THEN ST_ClosestPoint(p.geog::geometry, c.geog::geometry)::geography
  END geog_offset,
  CASE
     WHEN p.side IN ('left') THEN
       CASE
        WHEN c.highway = 'traffic_signals' AND c.traffic_signals_direction IN ('forward', 'backward') THEN ST_Buffer(ST_ClosestPoint(p.geog::geometry, c.geog::geometry)::geography, 10)
        WHEN c.crossing_kerb_extension = 'both' OR c.crossing_buffer_marking = 'both' THEN ST_Buffer(ST_ClosestPoint(p.geog::geometry, c.geog::geometry)::geography, 3)
        WHEN c.crossing = 'zebra' OR c.crossing_ref = 'zebra' OR c.crossing = 'traffic_signals' THEN ST_Buffer(ST_ClosestPoint(p.geog::geometry, c.geog::geometry)::geography, 4.5)
        WHEN c.crossing = 'marked' THEN ST_Buffer(ST_ClosestPoint(p.geog::geometry, c.geog::geometry)::geography, 2)
      END
    WHEN p.side IN ('right') THEN
      CASE
        WHEN c.highway = 'traffic_signals' AND c.traffic_signals_direction IN ('forward', 'backward') THEN ST_Buffer(ST_ClosestPoint(p.geog::geometry, c.geog::geometry)::geography, 10)
        WHEN c.crossing_kerb_extension = 'both' OR c.crossing_buffer_marking = 'both' THEN ST_Buffer(ST_ClosestPoint(p.geog::geometry, c.geog::geometry)::geography, 3)
        WHEN c.crossing = 'zebra' OR c.crossing_ref = 'zebra' OR c.crossing = 'traffic_signals' THEN ST_Buffer(ST_ClosestPoint(p.geog::geometry, c.geog::geometry)::geography, 4.5)
        WHEN c.crossing = 'marked' THEN ST_Buffer(ST_ClosestPoint(p.geog::geometry, c.geog::geometry)::geography, 2)
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
    (ST_Union(ST_Buffer(h.geog, 2)::geometry))::geography geog
  FROM
    highways h JOIN parking_lanes p ON st_intersects(p.geog, st_buffer(h.geog, 2))
  GROUP BY
    p.id
)
SELECT
  distinct p.id id,
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
          ST_SetSRID(coalesce(d.geog, 'GEOMETRYCOLLECTION EMPTY'::geography), 4326)::geometry
        ),
        ST_SetSRID(coalesce(c.geog, 'GEOMETRYCOLLECTION EMPTY'::geography), 4326)::geometry
      ),
      ST_SetSRID(coalesce(k.geog, 'GEOMETRYCOLLECTION EMPTY'::geography), 4326)::geometry
    ),
    ST_SetSRID(coalesce(hb.geog, 'GEOMETRYCOLLECTION EMPTY'::geography), 4326)::geometry
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
), dv as (
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


