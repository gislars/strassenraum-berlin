--transform to local SRS , we can use meters instead of degree for calculations
ALTER TABLE highways ALTER COLUMN geom TYPE geometry(LineString, 25833) USING ST_Transform(geom, 25833);
ALTER TABLE highways ADD COLUMN IF NOT EXISTS angle numeric;
UPDATE highways SET angle = degrees(ST_Azimuth(ST_StartPoint(ST_Transform(geom, 25833)), ST_EndPoint(ST_Transform(geom, 25833))));
DROP INDEX IF EXISTS highways_geom_idx;
CREATE INDEX highways_geom_idx ON public.highways USING gist (geom);

ALTER TABLE service ALTER COLUMN geom TYPE geometry(LineString, 25833) USING ST_Transform(geom, 25833);
ALTER TABLE service ADD COLUMN IF NOT EXISTS angle numeric;
UPDATE service SET angle = degrees(ST_Azimuth(ST_StartPoint(ST_Transform(geom, 25833)), ST_EndPoint(ST_Transform(geom, 25833))));
DROP INDEX IF EXISTS service_geom_idx;
CREATE INDEX service_geom_idx ON public.service USING gist (geom);

ALTER TABLE crossings ALTER COLUMN geom TYPE geometry(Point, 25833) USING ST_Transform(geom, 25833);
DROP INDEX IF EXISTS crossings_geom_idx;
CREATE INDEX crossings_geom_idx ON public.service USING gist (geom);

DROP TABLE IF EXISTS ped_crossings;
CREATE TABLE ped_crossings AS
SELECT
  row_number() over() id,
  c.node_id,
  pl.side,
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
    WHEN pl.side IN ('left') THEN h.parking_lane_left_width_carriageway
  WHEN pl.side IN ('right') THEN h.parking_lane_right_width_carriageway
  END "parking:lane:width:carriageway",
  h.parking_lane_left_width_carriageway "parking:lane:left:width:carriageway",
  h.parking_lane_right_width_carriageway "parking:lane:right:width:carriageway",
  c.geom geom,
  CASE
    WHEN pl.side IN ('left') THEN ST_ClosestPoint(pl.geom, c.geom)
    WHEN pl.side IN ('right') THEN ST_ClosestPoint(pl.geom, c.geom)
  END geom_offset,
  CASE
     WHEN pl.side IN ('left') THEN
       CASE
        WHEN c.highway = 'traffic_signals' AND c.traffic_signals_direction IN ('forward', 'backward') THEN ST_Buffer(ST_ClosestPoint(pl.geom, c.geom), 10)
        WHEN c.crossing_kerb_extension = 'both' OR c.crossing_buffer_marking = 'both' THEN ST_Buffer(ST_ClosestPoint(pl.geom, c.geom), 3)
        WHEN c.crossing = 'zebra' OR c.crossing_ref = 'zebra' OR c.crossing = 'traffic_signals' THEN ST_Buffer(ST_ClosestPoint(pl.geom, c.geom), 4.5)
        WHEN c.crossing = 'marked' THEN ST_Buffer(ST_ClosestPoint(pl.geom, c.geom), 2)
      END
    WHEN pl.side IN ('right') THEN
      CASE
        WHEN c.highway = 'traffic_signals' AND c.traffic_signals_direction IN ('forward', 'backward') THEN ST_Buffer(ST_ClosestPoint(pl.geom, c.geom), 10)
        WHEN c.crossing_kerb_extension = 'both' OR c.crossing_buffer_marking = 'both' THEN ST_Buffer(ST_ClosestPoint(pl.geom, c.geom), 3)
        WHEN c.crossing = 'zebra' OR c.crossing_ref = 'zebra' OR c.crossing = 'traffic_signals' THEN ST_Buffer(ST_ClosestPoint(pl.geom, c.geom), 4.5)
        WHEN c.crossing = 'marked' THEN ST_Buffer(ST_ClosestPoint(pl.geom, c.geom), 2)
      END
  END geom_offset_buffer
FROM
  (VALUES ('left'), ('right')) _(side)
  CROSS JOIN
  crossings c
  JOIN highways h ON ST_Intersects(c.geom, h.geom)
  JOIN parking_lanes_25833 pl ON pl.way_id = h.way_id
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
    WHEN highway = 'traffic_signals' AND "traffic_signals:direction" IN ('forward', 'backward') THEN ST_Buffer(geom, 10)
    WHEN "crossing:kerb_extension" = 'both' OR "crossing:buffer_marking" = 'both' THEN ST_Buffer(geom, 3)
    WHEN crossing = 'zebra' OR crossing_ref = 'zebra' OR crossing = 'traffic_signals' THEN ST_Buffer(geom, 4.5)
    WHEN crossing = 'marked' THEN ST_Buffer(geom, 2)
    --ELSE ST_Buffer(geom, 20)
  END geom
FROM
  ped_crossings
;

DROP TABLE IF EXISTS pre_parking_lanes_25833;
CREATE TABLE pre_parking_lanes_25833 AS
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
), highways_grouped as (
  SELECT
  max(id) id,
  ST_Union(geom) geom
  FROM
  highways
  GROUP BY
  type, surface, name, parking, parking_lane_left, parking_lane_right
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
    END capacity,
    CASE
      WHEN side = 'left' AND a.parking_lane_left_capacity IS NOT NULL THEN a.parking_lane_left_source_capacity
      WHEN side = 'right' AND a.parking_lane_right_capacity IS NOT NULL THEN a.parking_lane_right_source_capacity
    END "source:capacity",
    CASE WHEN side = 'left' THEN a.parking_lane_left_width_carriageway
         WHEN side = 'right' THEN a.parking_lane_right_width_carriageway
    END width,
    CASE WHEN side = 'left' THEN a.parking_lane_left_offset
         WHEN side = 'right' THEN a.parking_lane_right_offset
    END "offset",
    CASE
      WHEN a.parking_lane_left_offset NOT IN (0, 2.5, 5.5) AND side IN ('left') THEN ST_OffsetCurve(a.geom, a.parking_lane_left_offset)
      WHEN a.parking_lane_right_offset NOT IN (0, 2.5, 5.5) AND side IN ('right') THEN ST_OffsetCurve(a.geom, a.parking_lane_right_offset)
      -- workaround for "lwgeom_offsetcurve: noded geometry cannot be offset", only happens if value=2.5
      WHEN a.parking_lane_left_offset IN (2.5, 5.5) AND side IN ('left') THEN ST_OffsetCurve(a.geom, a.parking_lane_left_offset * 1.0001)
      WHEN a.parking_lane_right_offset IN (2.5, 5.5) AND side IN ('right') THEN ST_OffsetCurve(a.geom, a.parking_lane_right_offset * 1.0001)
    END geom,
    a.error_output
  FROM
    (VALUES ('left'), ('right')) _(side)
    CROSS JOIN
    highways a,
    dv
--  WHERE
--    (side = 'left' AND a.parking_lane_left NOT IN  ('no_stopping', 'no_parking', 'no'))
--    OR (side = 'right' AND a.parking_lane_right NOT IN  ('no_stopping', 'no_parking', 'no'))
;
ALTER TABLE pre_parking_lanes_25833 ALTER COLUMN geom TYPE geometry(MULTILINESTRING, 25833) USING st_multi(geom);
CREATE INDEX pre_parking_lanes_25833_geo_idx on pre_parking_lanes_25833 using gist (geom);

DROP TABLE IF EXISTS parking_lanes_25833;
CREATE TABLE parking_lanes_25833 AS
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
      WHEN side = 'left' AND a.parking_lane_left_capacity IS NULL THEN
        CASE
          WHEN a.parking_lane_left = 'parallel' AND ST_Length(a.geom) > dv.vehicle_length THEN floor((ST_Length(a.geom) + (dv.vehicle_dist_para - dv.vehicle_length)) / dv.vehicle_dist_para)
          WHEN a.parking_lane_left = 'diagonal' AND ST_Length(a.geom) > dv.vehicle_diag_width THEN floor((ST_Length(a.geom) + (dv.vehicle_dist_diag - dv.vehicle_diag_width)) / dv.vehicle_dist_diag)
          WHEN a.parking_lane_left = 'perpendicular' AND ST_Length(a.geom) > dv.vehicle_width THEN floor((ST_Length(a.geom) + (dv.vehicle_dist_perp - dv.vehicle_width)) / dv.vehicle_dist_perp)
        END
      WHEN side = 'right' AND a.parking_lane_right_capacity IS NOT NULL THEN a.parking_lane_right_capacity
      WHEN side = 'right' AND a.parking_lane_right_capacity IS NULL THEN
        CASE
          WHEN a.parking_lane_right = 'parallel' AND ST_Length(a.geom) > dv.vehicle_length THEN floor((ST_Length(a.geom) + (dv.vehicle_dist_para - dv.vehicle_length)) / dv.vehicle_dist_para)
          WHEN a.parking_lane_right = 'diagonal' AND ST_Length(a.geom) > dv.vehicle_diag_width THEN floor((ST_Length(a.geom) + (dv.vehicle_dist_diag - dv.vehicle_diag_width)) / dv.vehicle_dist_diag)
          WHEN a.parking_lane_right = 'perpendicular' AND ST_Length(a.geom) > dv.vehicle_width THEN floor((ST_Length(a.geom) + (dv.vehicle_dist_perp - dv.vehicle_width)) / dv.vehicle_dist_perp)
        END
    END capacity,
    CASE
      WHEN side = 'left' AND a.parking_lane_left_capacity IS NOT NULL THEN a.parking_lane_left_source_capacity
      WHEN side = 'left' AND a.parking_lane_left_capacity IS NULL THEN 'estimated'
      WHEN side = 'right' AND a.parking_lane_right_capacity IS NOT NULL THEN a.parking_lane_right_source_capacity
      WHEN side = 'right' AND a.parking_lane_right_capacity IS NULL THEN 'estimated'
    END "source:capacity",
    CASE WHEN side = 'left' THEN a.parking_lane_left_width_carriageway
         WHEN side = 'right' THEN a.parking_lane_right_width_carriageway
    END width,
    CASE WHEN side = 'left' THEN a.parking_lane_left_offset
         WHEN side = 'right' THEN a.parking_lane_right_offset
    END "offset",
    CASE
      WHEN a.parking_lane_left_offset NOT IN (0, 2.5, 5.5) AND side IN ('left') THEN ST_OffsetCurve(a.geom, a.parking_lane_left_offset)
      WHEN a.parking_lane_right_offset NOT IN (0, 2.5, 5.5) AND side IN ('right') THEN ST_OffsetCurve(a.geom, a.parking_lane_right_offset)
      -- workaround for "lwgeom_offsetcurve: noded geometry cannot be offset", only happens if value=2.5
      WHEN a.parking_lane_left_offset IN (2.5, 5.5) AND side IN ('left') THEN ST_OffsetCurve(a.geom, a.parking_lane_left_offset * 1.0001)
      WHEN a.parking_lane_right_offset IN (2.5, 5.5) AND side IN ('right') THEN ST_OffsetCurve(a.geom, a.parking_lane_right_offset * 1.0001)
    END geom,
    a.error_output
  FROM
    (VALUES ('left'), ('right')) _(side)
    CROSS JOIN
    highways a,
    dv
--  WHERE
--    (side = 'left' AND a.parking_lane_left NOT IN  ('no_stopping', 'no_parking', 'no'))
--    OR (side = 'right' AND a.parking_lane_right NOT IN  ('no_stopping', 'no_parking', 'no'))
;
ALTER TABLE parking_lanes_25833 ALTER COLUMN geom TYPE geometry(MULTILINESTRING, 25833) USING st_multi(geom);
CREATE INDEX parking_lanes_25833_geo_idx on parking_lanes_25833 using gist (geom);

/*DROP TABLE IF EXISTS parking_lanes_dissolved_25833;
CREATE TABLE parking_lanes_dissolved_25833 AS
SELECT
  row_number() over() id,
  a.side,
  a.highway,
  a."highway:name",
  a.parking,
  a.orientation,
  a."position",
  sum(a.capacity),
  a.width,
  a."offset",
  ST_Union(a.geom) geom
FROM
  parking_lanes_25833 a,
  parking_lanes_25833 b
WHERE
  --a.position IS NOT null
  ST_Intersects(a.geom, b.geom)
  --AND a.id != b.id
GROUP BY
  a.side,a.highway,a."highway:name",a."parking",a."orientation",a."position",a."width",a."offset"
;
ALTER TABLE parking_lanes_dissolved_25833 ALTER COLUMN geom TYPE geometry(MULTILINESTRING, 25833) USING ST_Multi(geom);
CREATE INDEX parking_lanes_dissolved_25833_geo_idx on parking_lanes_dissolved_25833 using gist (geom);*/

/*DROP TABLE IF EXISTS planes_25833;
CREATE TABLE planes_25833 AS
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
    CASE WHEN side = 'left' THEN a.parking_condition_left_maxstay
         WHEN side = 'right' THEN a.parking_condition_right_maxstay
    END maxstay,
    CASE WHEN side = 'left' THEN a.parking_lane_left_width_carriageway
         WHEN side = 'right' THEN a.parking_lane_right_width_carriageway
    END width,
    CASE WHEN side = 'left' THEN a.parking_lane_left_offset
         WHEN side = 'right' THEN a.parking_lane_right_offset
    END "offset",
    CASE
      WHEN a.parking_lane_left_offset NOT IN (0, 2.5, 5.5) AND side IN ('left') THEN ST_OffsetCurve(a.geom, a.parking_lane_left_offset)
      WHEN a.parking_lane_right_offset NOT IN (0, 2.5, 5.5) AND side IN ('right') THEN ST_OffsetCurve(a.geom, a.parking_lane_right_offset)
      -- workaround for "lwgeom_offsetcurve: noded geometry cannot be offset", only happens if value=2.5
      WHEN a.parking_lane_left_offset IN (2.5, 5.5) AND side IN ('left') THEN ST_OffsetCurve(a.geom, a.parking_lane_left_offset * 1.0001)
      WHEN a.parking_lane_right_offset IN (2.5, 5.5) AND side IN ('right') THEN ST_OffsetCurve(a.geom, a.parking_lane_right_offset * 1.0001)
    END geom,
    a.error_output
  FROM
    (VALUES ('left'), ('right')) _(side)
    CROSS JOIN
    highways a
;
ALTER TABLE planes_25833 ALTER COLUMN geom TYPE geometry(MULTILINESTRING, 25833) USING st_multi(geom);
CREATE INDEX planes_25833_geo_idx on planes_25833 using gist (geom);*/

/*DROP TABLE IF EXISTS highway_vertices;
CREATE TABLE highway_vertices AS
 WITH segments AS (
  SELECT *, ST_AsText(lag((pt).geom, 1, NULL) OVER (PARTITION BY id ORDER BY id, (pt).path)) lag_text, lag((pt).geom, 1, NULL) OVER (PARTITION BY id ORDER BY id, (pt).path), ST_AsText((pt).geom) geom_text, ST_AsText((pt).geom) text_geom, (pt).geom AS pt_geom
    FROM (SELECT * , ST_DumpPoints(geom) AS pt FROM highways) AS dumps
  )
  SELECT
    row_number() over() id,
    type,
    surface,
    name,
    parking,
    parking_lane_left,
    parking_lane_right,
    parking_lane_width_proc,
    parking_lane_width_effective,
    parking_lane_left_position,
    parking_lane_right_position,
    parking_lane_left_width,
    parking_lane_right_width,
    parking_lane_left_width_carriageway,
    parking_lane_right_width_carriageway,
    parking_lane_left_offset,
    parking_lane_right_offset,
    error_output,
    parking_condition_left,
    parking_condition_left_other,
    parking_condition_right,
    parking_condition_right_other,
    parking_condition_left_other_time,
    parking_condition_right_other_time,
    parking_condition_left_default,
    parking_condition_right_default,
    parking_condition_left_time_interval,
    parking_condition_right_time_interval,
    parking_condition_left_maxstay,
    parking_condition_right_maxstay,
    parking_lane_left_capacity,
    parking_lane_right_capacity,
    parking_lane_left_source_capacity,
    parking_lane_right_source_capacity,
    angle,
    pt_geom geom
  FROM segments WHERE pt_geom IS NOT NULL
;*/


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
  ST_Buffer(ST_Intersection(s.geom, h.geom), (h.parking_lane_width_proc / 2) + 5) geom
FROM service s
  JOIN highways h ON ST_Intersects(s.geom, h.geom)
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
  ST_Buffer(ST_Intersection(s.geom, pl.geom), GREATEST((s.parking_lane_width_proc / 2), 2) ) geom
FROM service s
  JOIN parking_lanes_25833 pl ON ST_Intersects(s.geom, pl.geom)
--WHERE
--  pl.position IS NOT NULL
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
  ST_CollectionExtract(ST_Intersection(a.geom, b.geom), 1) geom,
  ST_Buffer(ST_CollectionExtract(ST_Intersection(a.geom, b.geom), 1), 5) geom_buff
FROM
  parking_lanes_25833 a,
  parking_lanes_25833 b
WHERE
  ST_Intersects(a.geom, b.geom)
  AND a.id != b.id
  --AND (a.orientation NOT IN ('no','no_stopping','no_parking')
  --OR b.orientation NOT IN ('no','no_stopping','no_parking'))
  --AND a."highway" NOT IN ('pedestrian')
  --AND b."highway" NOT IN ('pedestrian')
;

DROP TABLE if exists pl;
CREATE TABLE pl AS
with temp as (
  select
  p.id,
  st_union(d.geom) geom
  from
    parking_lanes_25833 p join driveways d on st_intersects(d.geom, p.geom)
  group by
   p.id
)
SELECT
  p.*,
  st_difference(p.geom,ST_SetSRID(coalesce(t.geom, 'GEOMETRYCOLLECTION EMPTY'::geometry), 25833)) as geom_diff
FROM
  parking_lanes_25833 p left join temp t on p.id = t.id
;


DROP TABLE if exists pl_dev;
CREATE TABLE pl_dev AS
with driveways as (
  select
  p.id,
  st_union(d.geom) geom
  from
    parking_lanes_25833 p join driveways d on st_intersects(d.geom, p.geom)
  group by
   p.id
), crossings as (
  SELECT
  p.id,
  st_union(c.geom_offset_buffer) geom
  FROM
  ped_crossings c join parking_lanes_25833 p on st_intersects(p.geom, c.geom_offset_buffer)
  group by
   p.id
), kerb_intersections as (
  SELECT
  p.id,
  st_union(k.geom_buff) geom
  FROM
  kerb_intersection_points k join parking_lanes_25833 p on st_intersects(p.geom, k.geom_buff)
  where
  crossing_debug not in ('same_street')
  group by
   p.id
), highways_buffer as (
  SELECT
  h.way_id,
  st_buffer(h.geom, 2) geom
  FROM
    highways h join parking_lanes_25833 p on st_intersects(p.geom, st_buffer(h.geom, 2))
)
SELECT
  p.*,
  --st_difference(p.geom, ST_SetSRID(coalesce(c.geom, 'GEOMETRYCOLLECTION EMPTY'::geometry), 25833)) as geom_diff
  st_difference(
    st_difference(
    st_difference(
      st_difference(
      p.geom,
      ST_SetSRID(coalesce(d.geom, 'GEOMETRYCOLLECTION EMPTY'::geometry), 25833)
      ),
      ST_SetSRID(coalesce(c.geom, 'GEOMETRYCOLLECTION EMPTY'::geometry), 25833)
    ), ST_SetSRID(coalesce(k.geom, 'GEOMETRYCOLLECTION EMPTY'::geometry), 25833)
  ), ST_SetSRID(coalesce(hb.geom, 'GEOMETRYCOLLECTION EMPTY'::geometry), 25833)
  ) as geom_diff
FROM
  parking_lanes_25833 p left join driveways d on p.id = d.id
  left join crossings c on p.id = c.id
  left join kerb_intersections k on p.id = k.id
  left join highways_buffer hb on p.way_id = hb.way_id
;

/*DROP TABLE IF EXISTS kerb_intersection_points_buffer;
CREATE TABLE kerb_intersection_points_buffer AS
SELECT
  a."type",
  a."surface",
  a."name",
  a."parking_lane_left",
  a."parking_lane_right",
  a."parking_lane_width_proc",
  a."parking_lane_width_effective",
  a."parking_lane_left_position",
  a."parking_lane_right_position",
  a."parking_lane_left_width",
  a."parking_lane_right_width",
  a."parking_lane_left_width_carriageway",
  a."parking_lane_right_width_carriageway",
  a."parking_lane_left_offset",
  a."parking_lane_right_offset",
  a."error_output",
  ST_Buffer(ST_Intersection(a.geom, b.geom), 1) geom
FROM
  highways a,
  highways b
WHERE
  ST_Intersects(a.geom, b.geom)
  AND a.way_id != b.way_id
;*/

/*DROP TABLE IF EXISTS kerb_intersection_dissolve_points;
CREATE TABLE kerb_intersection_dissolve_points AS
SELECT
  a.id,
  a."highway" AS "type",
  --a.surface,
  a."highway:name" AS "name",
  a.orientation orientation,

  a.position parking_lane_left_position,
  a.position parking_lane_right_position,
  a.width parking_lane_left_width,
  a.width parking_lane_right_width,
  a.width parking_lane_left_width_carriageway,
  a.width parking_lane_right_width_carriageway,
  a.offset parking_lane_left_offset,
  a.offset parking_lane_right_offset,
  --a.error_output,
  --ST_Distance(a.geom, b.geom) distance,
  ST_CollectionExtract(ST_Intersection(a.geom, b.geom), 1) geom
FROM
  parking_lanes_dissolved_25833 a,
  parking_lanes_dissolved_25833 b
WHERE
  ST_Intersects(a.geom, b.geom)
  AND a.id != b.id
;*/

drop table if exists highway_split;
create table highway_split as
select
  row_number() over() nid,
  *,
  st_length((st_dump(geom_diff)).geom) length,
  (st_dump(geom_diff)).geom geoms
from
  pl_dev
--where st_length((st_dump(geom_diff)).geom) > 4.4