use wasm_bindgen::prelude::*;
use geo::{Point, Polygon};
use h3o::{CellIndex, Resolution};
use geojson::{Feature, FeatureCollection, GeoJson};
use serde::{Deserialize, Serialize};

#[wasm_bindgen]
pub struct GeoDashboard {
    data: Vec<GeoPoint>,
}

#[derive(Serialize, Deserialize)]
struct GeoPoint {
    lat: f64,
    lng: f64,
    value: f64,
}

#[wasm_bindgen]
impl GeoDashboard {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        Self { data: Vec::new() }
    }

    pub fn add_point(&mut self, lat: f64, lng: f64, value: f64) {
        self.data.push(GeoPoint { lat, lng, value });
    }

    pub fn hexagon_grid(&self, resolution: u8) -> JsValue {
        let mut hexagons = std::collections::HashMap::new();
        
        // Group points by hexagon
        for point in &self.data {
            let cell = CellIndex::from_coordinates(
                (point.lat, point.lng).into(),
                Resolution::try_from(resolution).unwrap()
            ).unwrap();
            
            let entry = hexagons.entry(cell).or_insert(0.0);
            *entry += point.value;
        }
        
        // Convert to GeoJSON
        let features: Vec<Feature> = hexagons.iter().map(|(cell, value)| {
            let boundary = cell.boundary();
            let coordinates: Vec<Vec<Vec<f64>>> = vec![
                boundary.iter().map(|coord| vec![coord.longitude(), coord.latitude()]).collect()
            ];
            
            let geometry = geojson::Geometry::new(
                geojson::Value::Polygon(coordinates)
            );
            
            let mut properties = geojson::JsonObject::new();
            properties.insert("value".to_string(), (*value).into());
            
            Feature {
                bbox: None,
                geometry: Some(geometry),
                id: None,
                properties: Some(properties),
                foreign_members: None,
            }
        }).collect();
        
        let collection = FeatureCollection {
            bbox: None,
            features,
            foreign_members: None,
        };
        
        let geojson = GeoJson::FeatureCollection(collection);
        serde_wasm_bindgen::to_value(&geojson).unwrap()
    }
}

// Add to GeoDashboard impl
#[wasm_bindgen]
pub fn heatmap(&self) -> JsValue {
    // Create a heatmap representation of the data
    let features: Vec<Feature> = self.data.iter().map(|point| {
        let geometry = geojson::Geometry::new(
            geojson::Value::Point(vec![point.lng, point.lat])
        );
        
        let mut properties = geojson::JsonObject::new();
        properties.insert("intensity".to_string(), point.value.into());
        
        Feature {
            bbox: None,
            geometry: Some(geometry),
            id: None,
            properties: Some(properties),
            foreign_members: None,
        }
    }).collect();
    
    let collection = FeatureCollection {
        bbox: None,
        features,
        foreign_members: None,
    };
    
    let geojson = GeoJson::FeatureCollection(collection);
    serde_wasm_bindgen::to_value(&geojson).unwrap()
}

#[wasm_bindgen]
pub fn cluster_points(&self, distance_threshold: f64) -> JsValue {
    // Simple clustering algorithm
    let mut clusters = Vec::new();
    let mut assigned = vec![false; self.data.len()];
    
    for i in 0..self.data.len() {
        if assigned[i] {
            continue;
        }
        
        let mut cluster = Vec::new();
        cluster.push(i);
        assigned[i] = true;
        
        for j in 0..self.data.len() {
            if i == j || assigned[j] {
                continue;
            }
            
            let p1 = &self.data[i];
            let p2 = &self.data[j];
            
            // Calculate distance (simplified)
            let dlat = p1.lat - p2.lat;
            let dlng = p1.lng - p2.lng;
            let distance = (dlat * dlat + dlng * dlng).sqrt() * 111000.0; // rough meters
            
            if distance <= distance_threshold {
                cluster.push(j);
                assigned[j] = true;
            }
        }
        
        clusters.push(cluster);
    }
    
    // Convert clusters to GeoJSON
    let features: Vec<Feature> = clusters.iter().map(|cluster| {
        // Calculate cluster center and total value
        let mut center_lat = 0.0;
        let mut center_lng = 0.0;
        let mut total_value = 0.0;
        let mut count = 0.0;
        
        for &idx in cluster {
            center_lat += self.data[idx].lat;
            center_lng += self.data[idx].lng;
            total_value += self.data[idx].value;
            count += 1.0;
        }
        
        center_lat /= count;
        center_lng /= count;
        
        let geometry = geojson::Geometry::new(
            geojson::Value::Point(vec![center_lng, center_lat])
        );
        
        let mut properties = geojson::JsonObject::new();
        properties.insert("count".to_string(), cluster.len().into());
        properties.insert("value".to_string(), total_value.into());
        properties.insert("avg_value".to_string(), (total_value / count).into());
        
        Feature {
            bbox: None,
            geometry: Some(geometry),
            id: None,
            properties: Some(properties),
            foreign_members: None,
        }
    }).collect();
    
    let collection = FeatureCollection {
        bbox: None,
        features,
        foreign_members: None,
    };
    
    let geojson = GeoJson::FeatureCollection(collection);
    serde_wasm_bindgen::to_value(&geojson).unwrap()
}

#[wasm_bindgen]
pub fn kring_analysis(&self, lat: f64, lng: f64, k: u32, resolution: u8) -> JsValue {
    // Get the center cell
    let center_cell = CellIndex::from_coordinates(
        (lat, lng).into(),
        Resolution::try_from(resolution).unwrap()
    ).unwrap();
    
    // Get the k-ring of hexagons around the center
    let ring = center_cell.grid_disk(k);
    
    // Calculate values for each hexagon in the ring
    let mut hexagons = std::collections::HashMap::new();
    
    for point in &self.data {
        let cell = CellIndex::from_coordinates(
            (point.lat, point.lng).into(),
            Resolution::try_from(resolution).unwrap()
        ).unwrap();
        
        if ring.contains(&cell) {
            let entry = hexagons.entry(cell).or_insert(0.0);
            *entry += point.value;
        }
    }
    
    // Convert to GeoJSON
    let features: Vec<Feature> = ring.iter().map(|&cell| {
        let boundary = cell.boundary();
        let coordinates: Vec<Vec<Vec<f64>>> = vec![
            boundary.iter().map(|coord| vec![coord.longitude(), coord.latitude()]).collect()
        ];
        
        let geometry = geojson::Geometry::new(
            geojson::Value::Polygon(coordinates)
        );
        
        let mut properties = geojson::JsonObject::new();
        let value = hexagons.get(&cell).copied().unwrap_or(0.0);
        properties.insert("value".to_string(), value.into());
        
        // Calculate distance from center (in k-rings)
        let distance = center_cell.grid_distance(cell).unwrap_or(0);
        properties.insert("distance".to_string(), distance.into());
        
        Feature {
            bbox: None,
            geometry: Some(geometry),
            id: None,
            properties: Some(properties),
            foreign_members: None,
        }
    }).collect();
    
    let collection = FeatureCollection {
        bbox: None,
        features,
        foreign_members: None,
    };
    
    let geojson = GeoJson::FeatureCollection(collection);
    serde_wasm_bindgen::to_value(&geojson).unwrap()
}

#[wasm_bindgen]
pub fn spatial_aggregation(&self, resolution: u8, aggregation_type: &str) -> JsValue {
    let mut hexagons = std::collections::HashMap::new();
    
    // Group points by hexagon
    for point in &self.data {
        let cell = CellIndex::from_coordinates(
            (point.lat, point.lng).into(),
            Resolution::try_from(resolution).unwrap()
        ).unwrap();
        
        let entry = hexagons.entry(cell).or_insert(Vec::new());
        entry.push(point.value);
    }
    
    // Convert to GeoJSON with aggregated values
    let features: Vec<Feature> = hexagons.iter().map(|(cell, values)| {
        let boundary = cell.boundary();
        let coordinates: Vec<Vec<Vec<f64>>> = vec![
            boundary.iter().map(|coord| vec![coord.longitude(), coord.latitude()]).collect()
        ];
        
        let geometry = geojson::Geometry::new(
            geojson::Value::Polygon(coordinates)
        );
        
        let mut properties = geojson::JsonObject::new();
        
        // Calculate aggregated value based on type
        let aggregated_value = match aggregation_type {
            "sum" => values.iter().sum::<f64>(),
            "avg" => {
                if values.is_empty() {
                    0.0
                } else {
                    values.iter().sum::<f64>() / values.len() as f64
                }
            },
            "max" => {
                if values.is_empty() {
                    0.0
                } else {
                    *values.iter().max_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)).unwrap()
                }
            },
            "min" => {
                if values.is_empty() {
                    0.0
                } else {
                    *values.iter().min_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)).unwrap()
                }
            },
            "count" => values.len() as f64,
            _ => values.iter().sum::<f64>(), // default to sum
        };
        
        properties.insert("value".to_string(), aggregated_value.into());
        properties.insert("count".to_string(), values.len().into());
        
        Feature {
            bbox: None,
            geometry: Some(geometry),
            id: None,
            properties: Some(properties),
            foreign_members: None,
        }
    }).collect();
    
    let collection = FeatureCollection {
        bbox: None,
        features,
        foreign_members: None,
    };
    
    let geojson = GeoJson::FeatureCollection(collection);
    serde_wasm_bindgen::to_value(&geojson).unwrap()
}

#[wasm_bindgen]
pub fn nearest_neighbor_analysis(&self, lat: f64, lng: f64, k: usize) -> JsValue {
    // Find k nearest neighbors to the given point
    let target = (lat, lng);
    
    // Calculate distances
    let mut points_with_distances: Vec<(usize, f64)> = self.data.iter().enumerate().map(|(idx, point)| {
        let dlat = point.lat - target.0;
        let dlng = point.lng - target.1;
        let distance = (dlat * dlat + dlng * dlng).sqrt() * 111000.0; // rough meters
        (idx, distance)
    }).collect();
    
    // Sort by distance
    points_with_distances.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
    
    // Take k nearest
    let nearest: Vec<usize> = points_with_distances.iter()
        .take(k.min(points_with_distances.len()))
        .map(|(idx, _)| *idx)
        .collect();
    
    // Convert to GeoJSON
    let features: Vec<Feature> = nearest.iter().map(|&idx| {
        let point = &self.data[idx];
        
        let geometry = geojson::Geometry::new(
            geojson::Value::Point(vec![point.lng, point.lat])
        );
        
        let mut properties = geojson::JsonObject::new();
        properties.insert("value".to_string(), point.value.into());
        properties.insert("distance".to_string(), points_with_distances.iter()
            .find(|(i, _)| *i == idx)
            .map(|(_, d)| *d)
            .unwrap_or(0.0).into());
        
        Feature {
            bbox: None,
            geometry: Some(geometry),
            id: None,
            properties: Some(properties),
            foreign_members: None,
        }
    }).collect();
    
    let collection = FeatureCollection {
        bbox: None,
        features,
        foreign_members: None,
    };
    
    let geojson = GeoJson::FeatureCollection(collection);
    serde_wasm_bindgen::to_value(&geojson).unwrap()
}

