require('dotenv').config();

const mineflayer = require('mineflayer');
const Jimp = require('jimp');

const bot = mineflayer.createBot({
    host: process.env.HOST,
    port: process.env.PORT,
    username: process.env.USERNAME,
    auth: process.env.AUTH
});

bot.on('login', () => {
    console.log('Steve Ross bot has succesfully connected to the server');
    createPixelArt('./reference/image.jpg');
});

async function createPixelArt(imagePath) {

    const image = await Jimp.read(imagePath);
    const width = image.bitmap.width;
    const height = image.bitmap.height;

    for (let y = 0; y < height; y++) {

        for (let x = 0; x < width; x++) {

            const color = Jimp.intToRGBA(image.getPixelColor(x, y));
            const blockType = getBlockType(color);

            if (blockType) {
                const position = bot.entity.position.offset(x, 0, -y);
                bot.placeBlock(bot.blockAt(position), new mineflayer.Vec3(0, 1, 0), (err) => {
                    if (err) console.log(err);
                });
            }
        }
    }
}

function getBlockType(color) {
    if (color.r === 255 && color.g === 0 && color.b === 0) return 'minecraft:red_wool';
    if (color.r === 0 && color.g === 255 && color.b === 0) return 'minecraft:green_wool';
    if (color.r === 0 && color.g === 0 && color.b === 255) return 'minecraft:blue_wool';
    if (color.r === 255 && color.g === 255 && color.b === 0) return 'minecraft:yellow_wool';
    if (color.r === 255 && color.g === 165 && color.b === 0) return 'minecraft:orange_wool';
    if (color.r === 128 && color.g === 0 && color.b === 128) return 'minecraft:purple_wool';
    if (color.r === 139 && color.g === 69 && color.b === 19) return 'minecraft:brown_wool';
    if (color.r === 255 && color.g === 255 && color.b === 255) return 'minecraft:white_wool';
    if (color.r === 0 && color.g === 0 && color.b === 0) return 'minecraft:black_wool';
    return null;
}

bot.on('spawn', () => {
    bot.chat('Hello! I am Steve Ross, a bot that can accurately render pixel art in Minecraft!');
});

bot.on('chat', (username, message) => {
    if (message === 'hi') {
        bot.chat(`Hello ${username}!`);
    }
});

bot.on('end', () => {
    console.log('Bot disconnected');
});