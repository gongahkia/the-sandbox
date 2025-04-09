let deathCount = 0;
let currentBoss = null;
let isInBossFight = false;

// ----- helper functions -----

async function extractBossName(info) {
  return new Promise((resolve) => {
    overwolf.media.getScreenshotUrl({
      crop: {
        x: 0,
        y: -0.3, 
        width: -1.0, 
        height: 0.3 
      },
      roundAwayFromZero: "true"
    }, async function(result) {
      if (result.status === "success") {
        try {
          const image = new Image();
          image.src = result.url;
          await new Promise(imgResolve => {
            image.onload = imgResolve;
          });
          const { createWorker } = Tesseract;
          const worker = await createWorker();
          await worker.loadLanguage('eng');
          await worker.initialize('eng');
          await worker.setParameters({
            tessedit_char_whitelist: 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789 -',
          });
          const { data } = await worker.recognize(image);
          await worker.terminate();
          const lines = data.text.split('\n');
          for (const line of lines) {
            if (line.length > 3 && !line.includes("HP") && !line.includes("%")) {
              resolve(line.trim());
              return;
            }
          }
          resolve("Unknown Boss");
        } catch (error) {
          console.error("OCR processing error:", error);
          resolve("Unknown Boss");
        }
      } else {
        console.error("Screenshot failed:", result.error);
        resolve("Unknown Boss");
      }
    });
  });
}

function saveStats(bossName, deaths) {
  console.log(`Boss fight complete: ${bossName} - Deaths: ${deaths}`);
}

// ----- event listener code -----

overwolf.games.events.onNewEvents.addListener(function(event) {
  if (event && event.events) {
    for (const gameEvent of event.events) {
      if (gameEvent.name === "match_start") {
        isInBossFight = true;
        currentBoss = extractBossName(gameEvent);
        deathCount = 0;
        updateDisplay();
      }
      if (gameEvent.name === "death") {
        if (isInBossFight) {
          deathCount++;
          updateDisplay();
        }
      }
      if (gameEvent.name === "match_end") {
        if (isInBossFight) {
          saveStats(currentBoss, deathCount);
          isInBossFight = false;
          currentBoss = null;
        }
      }
    }
  }
});

overwolf.games.events.onInfoUpdates2.addListener(function(info) {
});

function updateDisplay() {
  const deathCounter = document.getElementById('death-counter');
  const bossNameElement = document.getElementById('boss-name');
  if (deathCounter && bossNameElement) {
    deathCounter.innerText = deathCount;
    bossNameElement.innerText = currentBoss || "Unknown Boss";
  }
}