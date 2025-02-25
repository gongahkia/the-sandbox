import * as webllm from "@mlc-ai/web-llm";

let engine;

async function initializeEngine() {
    const initProgressCallback = (report) => {
        console.log(report.text);
    };

    const selectedModel = "Llama-3-8B-Instruct-q4f32_1";
    engine = await webllm.CreateMLCEngine(selectedModel, {
        initProgressCallback: initProgressCallback
    });

    console.log("Engine initialized");
}

async function init() {
    await initializeEngine();
    document.querySelector('#send-button').disabled = false;
}

async function generateResponse(userMessage) {
    const messages = [
        { role: "system", content: "You are a helpful AI assistant." },
        { role: "user", content: userMessage }
    ];

    const reply = await engine.chat.completions.create({ messages });
    return reply.choices[0].message.content;
}

window.sendMessage = async function() {
    try {
        const userInput = document.getElementById("user-input");
        const chatMessages = document.getElementById("chat-messages");

        const userMessage = userInput.value;
        chatMessages.innerHTML += `<p><strong>You:</strong> ${userMessage}</p>`;
        userInput.value = "";

        chatMessages.innerHTML += `<p><strong>AI:</strong> Generating response...</p>`;
        const aiResponse = await generateResponse(userMessage);
        chatMessages.lastElementChild.innerHTML = `<p><strong>AI:</strong> ${aiResponse}</p>`;
    } catch (error) {
        console.error("Error generating response:", error);
        chatMessages.innerHTML += `<p><strong>Error:</strong> Unable to generate response.</p>`;
    }
}

document.addEventListener('DOMContentLoaded', init);
document.querySelector('#send-button').addEventListener('click', sendMessage);