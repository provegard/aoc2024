const fs = require("fs")

const North = 0
const moveDeltas = [{ x: 0, y: -1 }, { x: 1, y: 0 }, { x: 0, y: 1 }, { x: -1, y: 0 }]

const isStart = c => c.ch === "^"
const isWall = c => c.ch === "#"
const posKey = pos => `${pos.y};${pos.x}`
const turnRight = direction => (direction + 1) % 4

const filename = "input"
const rows = fs.readFileSync(filename, { encoding: "ascii" }).split("\n")

const cells = rows.flatMap((row, y) => Array.from(row.trim()).map((ch, x) => ({ pos: { x, y }, ch })))
const entries = cells.map(c => [posKey(c.pos), c])
const cellMap = new Map(entries)
const startCell = Array.from(cellMap.values()).find(isStart)

function part2() {
    const positionsWithDir = walk(startCell.pos, North)
    const unique = distinctBy(positionsWithDir, x => posKey(x.pos))

    const obstacleSet = new Set()

    // Start at 1 to skip starting point
    for (let i = 1; i < unique.length; i++) {
        const obstaclePos = unique[i].pos
        const start = unique[i - 1]

        const visitedCount = new Map()
        const it = walk(start.pos, start.direction, obstaclePos)
        for (const p of it) {
            // skipping unique[0] (the starting point) makes no difference
            const key = `${posKey(p.pos)};${p.direction}`
            const current = visitedCount.get(key) ?? 0
            if (current >= 1) {
                // loop
                obstacleSet.add(posKey(obstaclePos))
                break
            }
            visitedCount.set(key, current + 1)
        }
    }

    console.log(obstacleSet.size)
}

part2() // 1770


function* walk(pos, direction, obstaclePos) {
    while (true) {
        yield { pos, direction }
        const nextPos = move(pos, direction)
        const nextCell = cellAt(nextPos)
        if (!nextCell) break // walked outside the grid
        if (isWall(nextCell) || eqPos(nextPos, obstaclePos)) {
            direction = turnRight(direction)
        } else {
            // move
            pos = nextPos
        }
    }
}

function eqPos(pos1, pos2) {
    if (!pos1 || !pos2) return false
    return pos1.x === pos2.x && pos1.y === pos2.y
}

function cellAt(pos) {
    return cellMap.get(posKey(pos))
}

function move(pos, direction) {
    const d = moveDeltas[direction]
    return { x: pos.x + d.x, y: pos.y + d.y }
}

function distinctBy(list, keyFn) {
    // Could possibly be simplified by using Map, but we cannot rely on Map iteration order.
    const keys = new Set()
    const result = []
    for (const item of list) {
        const key = keyFn(item)
        if (!keys.has(key)) {
            keys.add(key)
            result.push(item)
        }
    }
    return result
}