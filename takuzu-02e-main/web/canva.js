// Demo Canvas (JavaScript)

Module.onRuntimeInitialized = () => { start(); }
/* ******************** global variables ******************** */

var canvas = document.getElementById('mycanvas');
var ctx = canvas.getContext('2d');

var g;
var cols;
var rows;
var cell;

// load orange image
var orange = new Image();
orange.src = "image/orange.png";

// load orange image
var watermelon = new Image();
watermelon.src = "image/watermelon.png";


//add background
var background = new Image();
background.src = "image/background.jpg";


function start() {
    console.log("call start routine");
    g = Module._new_random(6, 6, false, false);
    cols = Module._nb_cols(g);
    rows = Module._nb_rows(g);


    drawCanvas();

    /* ******************** register events ******************** */

    window.addEventListener('load', windowLoad);              // window load
    canvas.addEventListener('click', canvasLeftClick);        // left click event
    canvas.addEventListener('contextmenu', canvasRightClick); // right click event

    function windowLoad() {
        console.log("window load");
        drawCanvas();
    }

}




/* ******************** event callback ******************** */



function canvasLeftClick(event) {
    event.preventDefault(); // prevent default context menu to appear...
    // get relative cursor position in canvas
    console.log("left click at position:", event.offsetX, event.offsetY);
    // update position of orange image used by drawCanvas()
    var sqY = event.offsetX / cell;
    var sqX = event.offsetY / cell;
    var squareVal = Module._get_square(g, sqX, sqY);
    if (squareVal == 0 || squareVal == 2) //S_EMPTY
        Module._play_move(g, sqX, sqY, 1);
    if (squareVal == 1)
        Module._play_move(g, sqX, sqY, 0);
    drawCanvas();
}

function canvasRightClick(event) {
    event.preventDefault(); // prevent default context menu to appear...
    // get relative cursor position in canvas
    console.log("right click at position:", event.offsetX, event.offsetY);
    // update position of orange image used by drawCanvas()
    var sqY = event.offsetX / cell;
    var sqX = event.offsetY / cell;
    var squareVal = Module._get_square(g, sqX, sqY);
    if (squareVal == 0 || squareVal == 1)
        Module._play_move(g, sqX, sqY, 2);
    if (squareVal == 2)
        Module._play_move(g, sqX, sqY, 0);
    drawCanvas();
}

function restart() {
    Module._restart(g);
    drawCanvas();
}

function undo() {
    Module._undo(g);
    drawCanvas();
}

function redo() {
    Module._redo(g);
    drawCanvas();
}

function solve() {
    Module._solve(g);
    drawCanvas();
}



/* ******************** canvas drawing ******************** */

function drawCanvas() {

    var width = canvas.width;
    var height = canvas.height;

    cell = width / cols;
    console.log("size = ", cell);

    // clear canvas
    ctx.clearRect(0, 0, width, height);

    ctx.save();
    ctx.drawImage(background, 0, 0, width, height);
    ctx.restore();

    // draw some lines
    for (i = 0; i < cols; i++) {
        ctx.save();
        ctx.moveTo(i * cell, 0);
        ctx.lineTo(i * cell, height);
        ctx.strokeStyle = 'blue';
        ctx.stroke(); // the stroke will actually paint the current lines
        ctx.restore();
    }

    //lignes horizontales
    for (j = 0; j < rows; j++) {
        ctx.save();
        ctx.moveTo(0, j * cell);
        ctx.lineTo(width, j * cell);
        ctx.strokeStyle = 'blue';
        ctx.stroke(); // the stroke will actually paint the current lines
        ctx.restore();
    }

    for (i = 0; i < cols; i++) {
        for (j = 0; j < rows; j++) {
            var number = Module._get_number(g, j, i);
            if (number == 1) {
                ctx.save();
                ctx.drawImage(orange, i * cell, j * cell, cell, cell);
                ctx.restore();
            }
            if (number == 0) {
                ctx.save();
                ctx.drawImage(watermelon, i * cell, j * cell, cell, cell);
                ctx.restore();
            }
        }
    }

    if (Module._is_over(g)) {
        ctx.fillStyle = "yellow";
        ctx.fillRect(0, 0, width, height);
        ctx.font = 'bold 40px Arial';
        ctx.fillStyle = 'black';
        ctx.textBaseline = 'middle';
        ctx.textAlign = 'center';
        ctx.fillText("Victory!", width / 2, height / 2);
    }

    ctx.restore();

}

// EOF