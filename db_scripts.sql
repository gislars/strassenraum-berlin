
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

