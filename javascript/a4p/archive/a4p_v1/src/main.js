console.log("balls");
alert("balls!");

// ----- HELPER FUNCTIONS -----

console.log("Starting to monitor and load all conversation card containers...");

const checkButtonAndClick = () => {
    const buttonSelector = "button.msg-overlay-bubble-header__button.truncate.ml2";
    const openMessagesTabButton = document.querySelector(buttonSelector);
    if (openMessagesTabButton) {
        console.log("Success: Clicking messages tab button");
        openMessagesTabButton.click();
        clearInterval(intervalId1); 
        console.log("Now waiting for the scrollable section to appear...");
        waitForScrollableSection();
    } else {
        console.log("Waiting for messages tab button to appear...");
    }
};

const waitForScrollableSection = () => {
    const scrollableContainerSelector = "section.scrollable.msg-overlay-list-bubble__content.msg-overlay-list-bubble__content--scrollable";
    const scrollableContainer = document.querySelector(scrollableContainerSelector);
    if (scrollableContainer) {
        console.log("Success: Scrollable section found.");
        clearInterval(intervalId2); 
        scrollAndLoadCards(scrollableContainer); 
    } else {
        console.log("Waiting for scrollable section to appear...");
    }
};

const scrollAndLoadCards = async (scrollableContainer) => {
    const cardContainerSelector = "div.msg-overlay-list-bubble__convo-card-container--v2";
    let previousHeight = 0;
    let profileDataArray = [];
    
    while (true) {
        const cards = document.querySelectorAll(cardContainerSelector);
        cards.forEach(card => {
            const profilePicElement = card.querySelector("div.presence-entity.presence-entity--size-3.msg-selectable-entity__entity img");
            const nameElement = card.querySelector("h3.msg-conversation-listitem__participant-names.msg-conversation-card__participant-names div.display-flex.justify-left.align-items-center.truncate span");
            const profilePicSrc = profilePicElement ? profilePicElement.src : null;
            const profileName = nameElement ? nameElement.innerText.trim() : null;
            if (profilePicSrc || profileName) {
                profileDataArray.push({ profilePicSrc, profileName });
            }
        });
        console.log(`Loaded ${profileDataArray.length} profiles so far...`);
        scrollableContainer.scrollTop = scrollableContainer.scrollHeight;
        await new Promise(resolve => setTimeout(resolve, 500));
        const currentHeight = scrollableContainer.scrollHeight;
        if (currentHeight === previousHeight) {
            console.log("All elements loaded, stopping scrolling...");
            break;
        }
        previousHeight = currentHeight;
    }
    console.log("Final Profile Data Array:", profileDataArray);
    return profileDataArray;
};
const intervalId1 = setInterval(checkButtonAndClick, 500);
let intervalId2;
const waitForScrollableSectionStart = () => {
    intervalId2 = setInterval(waitForScrollableSection, 500);
};
