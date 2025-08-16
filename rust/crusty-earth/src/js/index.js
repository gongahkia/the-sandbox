import init, { GeoDashboard } from '../pkg/geo_dashboard.js';

let map;
let geojsonLayer;
let dashboard;

async function initializeWasm() {
    await init();
    dashboard = new GeoDashboard();
    
    initializeMap();
    setupEventListeners();
}

function initializeMap() {
    map = L.map('map').setView([40.7128, -74.0060], 12);
    
    L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
    }).addTo(map);
    
    // Add info control
    const info = L.control();
    
    info.onAdd = function() {
        this._div = L.DomUtil.create('div', 'info');
        this.update();
        return this._div;
    };
    
    info.update = function(props) {
        this._div.innerHTML = '<h4>Hexagon Value</h4>' + 
            (props ? '<b>Value: ' + props.value.toFixed(2) + '</b>' : 'Hover over a hexagon');
    };
    
    info.addTo(map);
    
    // Store info control for later use
    window.infoControl = info;
}

function setupEventListeners() {
    document.getElementById('load-data').addEventListener('click', loadSampleData);
    document.getElementById('resolution').addEventListener('change', updateHexagonGrid);
}

function loadSampleData() {
    // Generate some random points around NYC
    const center = [40.7128, -74.0060];
    const points = 1000;
    
    for (let i = 0; i < points; i++) {
        const lat = center[0] + (Math.random() - 0.5) * 0.1;
        const lng = center[1] + (Math.random() - 0.5) * 0.1;
        const value = Math.random() * 100;
        
        dashboard.add_point(lat, lng, value);
    }
    
    updateHexagonGrid();
}

function updateHexagonGrid() {
    const resolution = parseInt(document.getElementById('resolution').value);
    const hexagons = dashboard.hexagon_grid(resolution);
    
    if (geojsonLayer) {
        map.removeLayer(geojsonLayer);
    }
    
    geojsonLayer = L.geoJSON(JSON.parse(hexagons), {
        style: function(feature) {
            const value = feature.properties.value;
            // Color scale from light to dark based on value
            const intensity = Math.min(255, Math.floor(value * 2.55));
            return {
                fillColor: `rgb(${255-intensity}, ${intensity}, 0)`,
                weight: 1,
                opacity: 0.8,
                color: '#666',
                fillOpacity: 0.7
            };
        },
        onEachFeature: function(feature, layer) {
            layer.on({
                mouseover: function(e) {
                    const layer = e.target;
                    layer.setStyle({
                        weight: 3,
                        color: '#666',
                        fillOpacity: 0.9
                    });
                    window.infoControl.update(feature.properties);
                },
                mouseout: function(e) {
                    geojsonLayer.resetStyle(e.target);
                    window.infoControl.update();
                }
            });
        }
    }).addTo(map);
    
    // Fit map to the hexagon layer bounds
    map.fitBounds(geojsonLayer.getBounds());
}

// Add to setupEventListeners function
document.getElementById('file-input').addEventListener('change', handleFileUpload);

// Add new function
function handleFileUpload(event) {
    const file = event.target.files[0];
    if (!file) return;
    
    const reader = new FileReader();
    
    reader.onload = function(e) {
        const contents = e.target.result;
        
        if (file.name.endsWith('.csv')) {
            processCSV(contents);
        } else if (file.name.endsWith('.geojson')) {
            processGeoJSON(contents);
        } else {
            alert('Unsupported file format. Please upload CSV or GeoJSON.');
        }
    };
    
    if (file.name.endsWith('.csv')) {
        reader.readAsText(file);
    } else if (file.name.endsWith('.geojson')) {
        reader.readAsText(file);
    }
}

function processCSV(contents) {
    // Simple CSV parsing - you might want to use a library for more complex CSVs
    const lines = contents.split('\n');
    const headers = lines[0].split(',');
    
    // Find lat/lng column indices
    const latIndex = headers.findIndex(h => h.toLowerCase().includes('lat'));
    const lngIndex = headers.findIndex(h => h.toLowerCase().includes('lon') || h.toLowerCase().includes('lng'));
    const valueIndex = headers.findIndex(h => h.toLowerCase().includes('value'));
    
    if (latIndex === -1 || lngIndex === -1) {
        alert('Could not find latitude/longitude columns in CSV');
        return;
    }
    
    // Clear existing data
    dashboard = new GeoDashboard();
    
    // Process data rows
    for (let i = 1; i < lines.length; i++) {
        if (!lines[i].trim()) continue;
        
        const columns = lines[i].split(',');
        const lat = parseFloat(columns[latIndex]);
        const lng = parseFloat(columns[lngIndex]);
        const value = valueIndex !== -1 ? parseFloat(columns[valueIndex]) : 1.0;
        
        if (!isNaN(lat) && !isNaN(lng)) {
            dashboard.add_point(lat, lng, value);
        }
    }
    
    updateHexagonGrid();
}

function processGeoJSON(contents) {
    try {
        const geojson = JSON.parse(contents);
        
        // Clear existing data
        dashboard = new GeoDashboard();
        
        // Process features
        if (geojson.type === 'FeatureCollection') {
            geojson.features.forEach(feature => {
                if (feature.geometry.type === 'Point') {
                    const lng = feature.geometry.coordinates[0];
                    const lat = feature.geometry.coordinates[1];
                    const value = feature.properties && feature.properties.value ? 
                                 parseFloat(feature.properties.value) : 1.0;
                    
                    dashboard.add_point(lat, lng, value);
                }
            });
            
            updateHexagonGrid();
        } else {
            alert('GeoJSON must be a FeatureCollection with Point features');
        }
    } catch (e) {
        alert('Error parsing GeoJSON: ' + e.message);
    }
}

initializeWasm();
