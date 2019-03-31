// Usage: node benchmark.js source [load] [--debug]

(async function () {
const _ = require('lodash');
const fs = require('fs');
const { ScreepsServer, TerrainMatrix } = require('screeps-server-mockup');

const sourcePath = process.argv[2];
let debug = false;
let minLoad = 1;
let maxLoad = 10;
let argIndex = 3;

if (process.argv.length > argIndex) {
    const arg = parseInt(process.argv[argIndex]);
    if (!isNaN(arg)) {
        maxLoad = arg;
        argIndex += 1;
    }
}

if (process.argv.length > argIndex) {
    if (process.argv[argIndex] == '--debug') {
        debug = true;
        minLoad = maxLoad;
        argIndex += 1;
    }
}

async function benchmark(source, load, stepLimit) {
    const server = new ScreepsServer();

    server.world.addCreep = async function(name, username, room, x, y, memory) {
        const { db, env } = this.server.common.storage;
        const user = await db.users.findOne({ username });
  
        const attributes = {
            name,
            body: [
                { type: 'work',  hits: 100 },
                { type: 'work',  hits: 100 },
                { type: 'work',  hits: 100 },
                { type: 'carry', hits: 100 },
                { type: 'move',  hits: 100 },
                { type: 'move',  hits: 100 },
                { type: 'move',  hits: 100 },
                { type: 'move',  hits: 100 },
            ],
            energy: 0,
            energyCapacity: 100,
            user: user._id,
            hits: 1,
            hitsMax: 1,
            spawning: false,
            fatigue: 0,
            notifyWhenAttacked: true,
        };
        const creepPromise = this.addRoomObject(
            room, 'creep', x, y, attributes
        );
  
        const key = env.keys.MEMORY + user._id;
        let userMemory = JSON.parse(await env.get(key));
        if(!userMemory.creeps) {
          userMemory.creeps = {};
        }
        userMemory.creeps[name] = memory;
        await env.set(key, JSON.stringify(userMemory));
  
        return await creepPromise;
    };

    const room = 'W0N1';

    try {
        // Initialize server
        await server.world.reset();

        // Create a new room with terrain and basic objects
        await server.world.addRoom(room);
        await server.world.setTerrain(room, new TerrainMatrix());
        await server.world.addRoomObject(
            room, 'controller', 10, 10, { level: 0 }
        );
        await server.world.addRoomObject(
            room, 'source', 10, 40, {
                energy: 1000,
                energyCapacity: 1000,
                ticksToRegeneration: 300,
            }
        );

        // Add a bot
        const bot = await server.world.addBot({
            username: 'bot',
            room,
            x: 25,
            y: 25,
            modules: { main: source },
        });

        if (debug) {
            // Print console logs every tick
            bot.on('console', (logs, results, userid, username) => {
                _.each(
                    logs, line => console.log(`[console|${username}]`, line)
                );
            });
        }

        async function addHarvester(i, j) {
            return await server.world.addCreep(
                'Creep' + i + ':' + j, 'bot', room, i, j, { role: 'harvester' }
            );
        }
        for(let i = 0; i < load; ++i) {
            let pos = 11 + i;
            await addHarvester(pos, 11);
            await addHarvester(pos, 39);
            await addHarvester(11, pos);
            await addHarvester(39, pos);
        }

        const { db } = server.common.storage;
        await db['rooms.objects'].update(
            { type: 'spawn' }, { $set: { energy: 0 } }
        );

        // Start server and run until the spawn is full
        await server.start();
        let times = [];
        if (!stepLimit) {
            stepLimit = Infinity;
        }
        for(let i = 0; i < stepLimit; ++i) {
            const start = new Date();
            await server.tick();
            times.push(new Date() - start);

            if (debug) {
                _.each(
                    await bot.newNotifications,
                    ({ message }) => console.log('[notification]', message)
                );
                const creeps = await db['rooms.objects'].find({
                    type: 'creep'
                });
                console.log(
                    '[creeps]', _.map(creeps, (creep) => [creep.x, creep.y])
                );
            }

            const spawn = await db['rooms.objects'].findOne({ type: 'spawn' });
            if (spawn.energy == spawn.energyCapacity) {
                break;
            }
        }
        return times;
    } finally {
        // Stop server and disconnect storage
        server.stop();
    }
}

async function benchmarkWithBaseline(bundle, load) {
    const bundleTimes = await benchmark(bundle, load);
    const baselineTimes = await benchmark('', load, bundleTimes.length);
    return _.zipWith(bundleTimes, baselineTimes, (t1, t2) => t1 - t2);
}

try {
    const source = fs.readFileSync(sourcePath, 'UTF-8');
    const benchmarkFn = !debug ? benchmarkWithBaseline : benchmark;
    for (let load = minLoad; load <= maxLoad; ++load) {
        const times = await benchmarkFn(source, load);
        const meanTime = _.mean(times.slice(1));
        console.log('load', load, ':', meanTime);
    }
} catch (err) {
    console.error(err);
} finally {
    // required as there is no way to properly shutdown storage :(
    process.exit();
}

}());
