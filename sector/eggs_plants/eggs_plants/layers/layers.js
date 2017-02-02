var baseLayer = new ol.layer.Group({
    'title': 'Base maps',
    layers: [
new ol.layer.Tile({
    'title': 'OSM',
    'type': 'base',
    source: new ol.source.OSM()
})
]
});
var format_eggs_plants = new ol.format.GeoJSON();
var features_eggs_plants = format_eggs_plants.readFeatures(geojson_eggs_plants, 
            {dataProjection: 'EPSG:4326', featureProjection: 'EPSG:2100'});
var jsonSource_eggs_plants = new ol.source.Vector();
jsonSource_eggs_plants.addFeatures(features_eggs_plants);var lyr_eggs_plants = new ol.layer.Vector({
                source:jsonSource_eggs_plants, 
                style: style_eggs_plants,
                title: "eggs_plants"
            });

lyr_eggs_plants.setVisible(true);
var layersList = [baseLayer,lyr_eggs_plants];
lyr_eggs_plants.set('fieldAliases', {'approval_code': 'approval_code', 'business_name': 'business_name', 'location': 'location', 'prefecture_name_gr': 'prefecture_name_gr', 'region_name_gr': 'region_name_gr', 'activity': 'activity', 'latitude': 'latitude', 'longitude': 'longitude', });
lyr_eggs_plants.set('fieldImages', {'approval_code': 'TextEdit', 'business_name': 'TextEdit', 'location': 'TextEdit', 'prefecture_name_gr': 'TextEdit', 'region_name_gr': 'TextEdit', 'activity': 'TextEdit', 'latitude': 'TextEdit', 'longitude': 'TextEdit', });
lyr_eggs_plants.set('fieldLabels', {'approval_code': 'no label', 'business_name': 'no label', 'location': 'no label', 'prefecture_name_gr': 'no label', 'region_name_gr': 'no label', 'activity': 'no label', 'latitude': 'no label', 'longitude': 'no label', });
