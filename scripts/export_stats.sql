WITH stats AS (
SELECT
  id,
  name,
  (
    SELECT
      id
    FROM
      boundaries_stats b1
    WHERE
      ST_Contains(b1.geom, ST_PointOnSurface(b.geom))
      AND b1.admin_level < b.admin_level
      AND b1.admin_level = 9
  ) parent_id ,
  (
    SELECT
      name
    FROM
      boundaries_stats b1
    WHERE
      ST_Contains(b1.geom, ST_PointOnSurface(b.geom))
      AND b1.admin_level < b.admin_level
      AND b1.admin_level = 9
  ) parent_name,
  admin_level,
  aera_sqkm,
  street_side_km,
  lane_km,
  d_other_km,
  sum_km,
  length_wo_dual_carriageway,
  done_percent,
  geom
FROM
  boundaries_stats b
WHERE
  b.name NOT IN ('Brandenburg')
ORDER BY b.admin_level, b.name
)
SELECT
  json_build_object(
    'type', 'FeatureCollection',
    'features', json_agg(ST_AsGeoJSON(t.*)::json)
  )
FROM
  stats t
;