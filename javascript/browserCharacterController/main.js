alert("tikrit begins");

// ---------- MAP ----------

mapData = {
  "1": `
@..................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
  `, 

  "2": `
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
..M................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
...................................
..................................@
  `
}

// ---------- LEGEND ----------

// CANVAS is 700 by 700 px
// each char is a 20 by 20 px (font size 20)
// each room is effectively a grid from (0,0) to (35,35)

// . => floor tiles
// @ => player (white), when dodge rolling become light gray
// X => solid wall indoor outdoor (yellow, dark gray or brown)
// [#, ¥] => trees (dark green)
// [~, ] => water
// = => bridge over gap or spikes or water (brown)
// ^ => spikes
// [",',`] => grass (mixes of bright and dark green)
// [+, -] => open door, closed door
// [Ø, Ö]  => locked door, cave entrance
// & => health pickup
// ! => weapon pickup
// % => item pickup
// ? => mystery chest
// ê => boss
// M => enemy mob
// R => enemy rampager
// S => enemy sniper
// B => enemy bomber

// !! DONT RENDER AIR BLOCK IN ACTUAL RENDER ENGINE (?) !!

// ---------- UTIL FUNCTIONS ----------

function convCoords(coords) {
  const scaleOffset = 20;
  const x = coords[0] * scaleOffset;
  const y = coords[1] * scaleOffset;
  return [x,y];
}

function parseMapData(entity, mapData) {
  var coordMap = {}
  const currRoom = entity.player.currRoom;
  // console.log(mapData[currRoom].trim().split("\n"));
  for (var y = 0; y < mapData[currRoom].trim().split("\n").length; y++) {
    for (var x = 0; x < mapData[currRoom].trim().split("\n")[y].length; x++) {
      coordMap[convCoords([x,y])] = mapData[currRoom].trim().split("\n")[y][x];
      // coordMap[[x,y]] = mapData[currRoom].trim().split("\n")[y][x];
    }
  }
  return coordMap;
}

// FUA add code to handle enemies rendering elegantly
// exclusively responsible for retrieving entity coordinates. inaminate objects (items) don't move so this doesn't matter
function getCoord(entity, coordMap) {
  // var coordMap = {...coordMap};
  for (var coord in coordMap) {
    switch (coordMap[coord]) {
      case "@":
        entity.player.coord = coord.split(",").map(Number);
        break;
      default:
        break;
    }
  }
}

function updateCoord(entity, coordMap) {
  for (var e in entity) {
    coordMap[entity[e].coord.join(",")] = e.model;
  }
}

function renderMapData(coordMap) {
  for (var coord in coordMap) {
    // console.log(typeof coord);
    // console.log(typeof coordMap[coord]);
    // console.log(coord, coordMap[coord]);
    c.fillText(coordMap[coord], parseInt(coord.trim().split(",")[0]), parseInt(coord.trim().split(",")[1]) + 20);
    // c.fillText(entity[e].model, entity[e].coord.x, entity[e].coord.y + entity[e].size);
  }
}

function checkBounds(coords, entity) {
  return coords[0] < 0 || coords[1] < 0 || coords[1] > c1.height - entity.player.size || coords[0] > c1.width - entity.player.size;
}

// ---------- CANVAS ----------

// instantiating canvas
const c1 = document.getElementById("c1");

// dynamically resizes canvas
/* 
c1.width = window.innerWidth;
c1.height = window.innerHeight;
*/

c1.width = 700;
c1.height = 700;

console.log(c1);
const c = c1.getContext("2d");
c.font = "20px 'Comic Sans MS', cursive";

// ---------- PREP WORK ----------

// ----- WORLD OBJECTS -----

// ----- ENTITIES ----- 

var entity = {
  player: {
    model: "@",
    coord: [],
    currRoom: 2,
    size: 20,
    speed: 20,
    health: 10,
    items: {
    }
  },

  enemy1 : {
    model: "M",
    coord: [],
    currRoom: 2,
    size: 20,
    speed: 15,
    health: 3,
    items: {

    }
  }
}

// ---------- EVENT LOOP ----------

function clearScreen() {
  c.clearRect(0, 0, c1.width, c1.height);
}

// FUA REWORK THIS LATER
var coordMap = parseMapData(entity, mapData); // called to update coordinates of entities
// console.log(finMapData);

// ---------- USER INPUT ----------

// movement => "wasd"
// use item => 'u', applicable on doors also
// throw item => 't'
// place block => 'p', drop item if not block
// pause game => 'q'

// FUA might have to debug this further => REMOVE USE OF COORDS ENTIRELY
document.addEventListener('keydown', function(event) {
  if (['w', 'a', 's', 'd', 'u', 'p', 'q'].includes(event.key)) {
    getCoord(entity, coordMap);
    
    // Save the updated coordinates in separate variables
    let newX = entity.player.coord[0];
    let newY = entity.player.coord[1];

    switch (event.key) {
      case 'w':
        newY -= entity.player.speed;
        break;
      case 'a':
        newX -= entity.player.speed;
        break;
      case 's':
        newY += entity.player.speed;
        break;
      case 'd':
        newX += entity.player.speed;
        break;
      // FUA: ADD ACTIONS FOR THESE 4 KEYS AS BELOW
      case 'u':
        break;
      case 'p':
        break;
      case 'q':
        break;
      default:
        console.log(`Invalid input detected. Keypress was ${event.key}`);
    }

    // Check bounds after updating the coordinates
    if (!checkBounds([newX, newY], entity)) {
      // Update the coordinates only if they are within bounds
      entity.player.coord = [newX, newY];
    }

    // Update the coordinate map after the check
    updateCoord(entity, coordMap);
  }
});

// FUA rework this section later
function eventLoop() {
  // debugger;
  console.log("neat");
  clearScreen();
  renderMapData(coordMap);
  requestAnimationFrame(eventLoop);
}

requestAnimationFrame(eventLoop);
