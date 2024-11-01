import {Chess} from 'chess.js';
import * as fs from "fs";
import {spawn} from "child_process";

const swipl = (/^win/.test(process.platform) ? "C:\\Program Files\\swipl\\bin\\swipl" : "swipl");
const project = process.argv.length > 2 ? process.argv[2] : 'tests/dummy.pl';

console.log(`testing: ${project}`)

const chess = new Chess()

const file = "tmp.pgn";

async function randomGame() {
    let error = false;
    let move = 1;
    while (!chess.isGameOver() && !error) {
        const moves = chess.moves()
        const choice = moves[Math.floor(Math.random() * moves.length)]
        chess.move(choice)
        fs.writeFileSync(file, chess.pgn());
        console.log(`Move ${move++}`);
        console.log("Bot:--------------------")
        console.log(chess.pgn())
        console.log("End data:---------------")

        await opponent().catch((e) => {printError(e); error = true;});
        console.log()
    }
    return !error;
}

function opponent() {
    return new Promise(function (resolve, reject) {
        const hrstart = process.hrtime();
        const engine = spawn(swipl, ["-t", "halt", "-f", "-q", "-O", project, file]);
        engine.stdout.on('data', check(resolve, reject, hrstart));
        engine.stderr.on('data', (e) => reject(e));
    });
}

function printError(e) {
    console.error('Error on output:--------');
    console.error(e.toString());
    console.error('End error:--------------');
}

function check(resolve, reject, hrstart) {
    return (data) => {
        //SWIPL prints newlines
        data = data.toString().replace("\r", "")

        const pgn = data.trim();
        try {
            chess.loadPgn(pgn);
            console.error("Your computer:----------")
            console.error(data)
            console.error("End data:---------------")
            resolve(process.hrtime(hrstart))
        } catch (e) {
            reject(e)
        }
    };
}

const success = await randomGame();
process.exit(success? 0 : 1)
