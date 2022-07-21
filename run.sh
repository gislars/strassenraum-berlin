#!/bin/sh

#http://download.geofabrik.de/europe/germany-latest.osm.pbf
OSM_DOWNLOAD_FILE=berlin-latest.osm.pbf
#OSM_DOWNLOAD_FILE=germany-latest.osm.pbf
OSM_DOWNLOAD_URL=http://download.geofabrik.de/europe/germany/${OSM_DOWNLOAD_FILE}

OSM2PGSQL_BIN=/usr/bin/osm2pgsql

OSM_WORKDIR="$(dirname "$0")/"
OSM_LOCAL_FILE=${OSM_WORKDIR}openstreetmap-latest.osm.pbf
OSM_FILTERED_FILE=${OSM_WORKDIR}openstreetmap-filtered.osm.pbf
OSM_FILTER_EXPRESSIONS=${OSM_WORKDIR}filter-expressions.txt
OSM_LUA_SCRIPT=${OSM_WORKDIR}highways.lua
OSM_POSTPROCESS_SCRIPT=${OSM_WORKDIR}db_scripts.sql

## using database credentials from ~/.pg_service.conf
export PGSERVICE=osmdb

#
echo "downloading ${OSM_DOWNLOAD_URL}"
wget -q -N --show-progress ${OSM_DOWNLOAD_URL}
cp ${OSM_DOWNLOAD_FILE} ${OSM_LOCAL_FILE}
OSM_TIMESTAMP=`osmium fileinfo ${OSM_LOCAL_FILE} -g header.option.timestamp`

if [ -f "${OSM_LOCAL_FILE}" ]; then
  echo "processing osm data"
  osmium tags-filter -O -o ${OSM_FILTERED_FILE} -e ${OSM_FILTER_EXPRESSIONS} ${OSM_LOCAL_FILE}
else
  echo "file ${OSM_LOCAL_FILE} or ${OSM_FILTER_EXPRESSIONS} not found"
fi

if [ -f "${OSM_FILTERED_FILE}" ]; then
  echo "import osm data into db"
  ${OSM2PGSQL_BIN} -c -O flex -S ${OSM_LUA_SCRIPT} ${OSM_FILTERED_FILE}
fi

if [ -f "${OSM_POSTPROCESS_SCRIPT}" ]; then
  echo "postprocess osm db data"
  psql -q -f "${OSM_POSTPROCESS_SCRIPT}"
  psql -q -c "COMMENT ON TABLE highways IS '${OSM_TIMESTAMP}';"
  psql -q -c "COMMENT ON TABLE boundaries_stats IS '${OSM_TIMESTAMP}';"
  psql -q -c "COMMENT ON TABLE parking_segments IS '${OSM_TIMESTAMP}';"
  psql -q -c "COMMENT ON TABLE parking_spaces IS '${OSM_TIMESTAMP}';"
  psql -q -t -f ${OSM_WORKDIR}scripts/export_stats.sql -o ${OSM_WORKDIR}export/boundaries_stats.geojson
else
  echo "import script not found"
fi