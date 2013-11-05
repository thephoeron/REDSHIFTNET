// Based on 'HTML5 Matrix Code Rain' by Collin Henderson (http://labs.syropia.net/)
// Purpose modded for artist site splash screen by "the Phoeron" (http://thephoeron.com/)

var textStrip = ['0', '1'];

var stripCount = 27, stripX = new Array(), stripY = new Array(), dY = new Array(), stripFontSize = new Array();

for (var i = 0; i < stripCount; i++) {
    stripX[i] = Math.floor(Math.random()*1280);
    stripY[i] = -100;
    dY[i] = Math.floor(Math.random()*7)+6;
    stripFontSize[i] = Math.floor(Math.random()*14)+10;
}

var theColors = ['#330000', '#660000', '#990000', '#be0000', '#db0000', '#ff0000'];

var elem, context, timer;

function drawStrip(x, y) {
    for (var k = 0; k <= 40; k++) {
        var randChar = textStrip[Math.floor(Math.random()*textStrip.length)];
        if (context.fillText) {
            switch (k) {
            case 0:
                context.fillStyle = theColors[0]; break;
            case 1:
                context.fillStyle = theColors[1]; break;
            case 3:
                context.fillStyle = theColors[2]; break;
            case 7:
                context.fillStyle = theColors[3]; break;
            case 13:
                context.fillStyle = theColors[4]; break;
            case 17:
                context.fillStyle = theColors[5]; break;
            }
            context.fillText(randChar, x, y);
        }
        y -= stripFontSize[k];
    }
}

function draw() {
    // clear the canvas and set the properties
    context.clearRect(0, 0, elem.width, elem.height);
    context.shadowOffsetX = context.shadowOffsetY = 0;
    context.shadowBlur = 8;
    context.shadowColor = '#ff0000';
    
    for (var j = 0; j < stripCount; j++) {
        context.font = stripFontSize[j]+'px \"ProFont Windows tweaked"';
        context.textBaseline = 'top';
        context.textAlign = 'center';
        
        if (stripY[j] > 1358) {
            stripX[j] = Math.floor(Math.random()*elem.width);
            stripY[j] = -100;
            dY[j] = Math.floor(Math.random()*7)+6;
            stripFontSize[j] = Math.floor(Math.random()*14)+10;
            drawStrip(stripX[j], stripY[j]);
        } else drawStrip(stripX[j], stripY[j]);
        
        stripY[j] += dY[j];
    }
}

function init() {
    // get the canvas' id
    elem = document.getElementById('theMatrix');
    if (!elem || !elem.getContext) return;
    
    // get the canvas' context
    context = elem.getContext('2d');
    if (!context) return;
    
    timer = setInterval('draw()', 90);
}
