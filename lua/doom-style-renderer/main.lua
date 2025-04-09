local map = {
    {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1},
    {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1},
    {1,0,0,0,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,1,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1},
    {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}
}

local player = {x = 2.5, y = 2.5, angle = 0, fov = math.pi / 3, speed = 2, turnSpeed = 2}
local textures = {}
local CELL_SIZE = 64

function love.load()
    textures[1] = love.graphics.newImage("bricks.png")
    love.window.setMode(800, 600)
    love.graphics.setDefaultFilter("nearest", "nearest")
end

function love.update(dt)
    local dx, dy = 0, 0
    if love.keyboard.isDown('w') then
        dx = math.cos(player.angle) * player.speed * dt
        dy = math.sin(player.angle) * player.speed * dt
    elseif love.keyboard.isDown('s') then
        dx = -math.cos(player.angle) * player.speed * dt
        dy = -math.sin(player.angle) * player.speed * dt
    end
    if map[math.floor(player.y)][math.floor(player.x + dx)] == 0 then
        player.x = player.x + dx
    end
    if map[math.floor(player.y + dy)][math.floor(player.x)] == 0 then
        player.y = player.y + dy
    end
    if love.keyboard.isDown('a') then
        player.angle = player.angle - player.turnSpeed * dt
    elseif love.keyboard.isDown('d') then
        player.angle = player.angle + player.turnSpeed * dt
    end
end

function castRay(x, y, angle)
    local rayDirX = math.cos(angle)
    local rayDirY = math.sin(angle)
    local mapX, mapY = math.floor(x), math.floor(y)
    local sideDistX, sideDistY
    local deltaDistX = math.abs(1 / rayDirX)
    local deltaDistY = math.abs(1 / rayDirY)
    local perpWallDist
    local stepX, stepY
    local hit = 0
    local side
    if rayDirX < 0 then
        stepX = -1
        sideDistX = (x - mapX) * deltaDistX
    else
        stepX = 1
        sideDistX = (mapX + 1.0 - x) * deltaDistX
    end
    if rayDirY < 0 then
        stepY = -1
        sideDistY = (y - mapY) * deltaDistY
    else
        stepY = 1
        sideDistY = (mapY + 1.0 - y) * deltaDistY
    end
    while hit == 0 do
        if sideDistX < sideDistY then
            sideDistX = sideDistX + deltaDistX
            mapX = mapX + stepX
            side = 0
        else
            sideDistY = sideDistY + deltaDistY
            mapY = mapY + stepY
            side = 1
        end
        if map[mapY][mapX] > 0 then hit = 1 end
    end
    if side == 0 then
        perpWallDist = (mapX - x + (1 - stepX) / 2) / rayDirX
    else
        perpWallDist = (mapY - y + (1 - stepY) / 2) / rayDirY
    end
    local wallX
    if side == 0 then
        wallX = y + perpWallDist * rayDirY
    else
        wallX = x + perpWallDist * rayDirX
    end
    wallX = wallX - math.floor(wallX)
    
    local texX = math.floor(wallX * CELL_SIZE)
    if (side == 0 and rayDirX > 0) or (side == 1 and rayDirY < 0) then
        texX = CELL_SIZE - texX - 1
    end
    return perpWallDist, texX, side
end

function love.draw()
    local w, h = love.graphics.getDimensions()
    for x = 0, w do
        local cameraX = 2 * x / w - 1
        local rayDirX = player.angle + player.fov * (x / w - 0.5)
        local dist, texX, side = castRay(player.x, player.y, rayDirX)
        local lineHeight = h / dist
        local drawStart = -lineHeight / 2 + h / 2
        if drawStart < 0 then drawStart = 0 end
        local drawEnd = lineHeight / 2 + h / 2
        if drawEnd >= h then drawEnd = h - 1 end
        love.graphics.setColor(side == 1 and {0.7, 0.7, 0.7} or {1, 1, 1})
        love.graphics.draw(textures[1], 
            love.graphics.newQuad(texX, 0, 1, CELL_SIZE, CELL_SIZE, CELL_SIZE),
            x, drawStart, 0, 1, (drawEnd - drawStart) / CELL_SIZE)
    end
    local mapWidth = #map[1]
    local mapHeight = #map
    local cellSize = 10
    local mapX = w - mapWidth * cellSize - 10
    local mapY = 10
    love.graphics.setColor(0, 0, 0, 0.7)
    love.graphics.rectangle("fill", mapX, mapY, mapWidth * cellSize, mapHeight * cellSize)
    for y = 1, mapHeight do
        for x = 1, mapWidth do
            if map[y][x] == 1 then
                love.graphics.setColor(1, 1, 1)
                love.graphics.rectangle("fill", mapX + (x-1) * cellSize, mapY + (y-1) * cellSize, cellSize, cellSize)
            end
        end
    end
    love.graphics.setColor(1, 0, 0)
    love.graphics.circle("fill", mapX + (player.x - 1) * cellSize, mapY + (player.y - 1) * cellSize, 3)
    local dirX = math.cos(player.angle) * 5
    local dirY = math.sin(player.angle) * 5
    love.graphics.line(
        mapX + (player.x - 1) * cellSize, 
        mapY + (player.y - 1) * cellSize, 
        mapX + (player.x - 1) * cellSize + dirX, 
        mapY + (player.y - 1) * cellSize + dirY
    )
end