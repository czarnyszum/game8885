const wsAddr = 'ws://127.0.0.1:8000/ws'

var blockCloseCallback = false;
var connectionEstablished = false;
var ws = new WebSocket(wsAddr, 'ui');

const FRAMEPERIOD = 500;

document.addEventListener("DOMContentLoaded", onLoad);


async function sendMsg(msg) {
    let m = JSON.stringify(msg);
//    console.log(m);
	
    await ws.send(m);
}

ws.onopen = async (event) => {

    connectionEstablished = true;
    
    let msg = "Init"; // {tag: "Init"};
    
    await sendMsg(msg);
};

ws.onmessage = async (event) => {
    
    let msg = JSON.parse(event.data);

    await dispatchCommand(msg);
};

ws.onclose = (event) => {

    connectionEstablished = false;

    if(!blockCloseCallback) {
 
    alert("Утеряна связь с сервером");

    const layout = document.getElementById('layout');
    layout.innerHTML = "<div class='msg-container err-bg'><h1>Утеряна связь с сервером</h1><button class='pure-button button18' onClick='window.location.reload();' >Перезагрузить страницу</button></div>"

    }
}

async function dispatchCommand(obj) {
    
    switch (obj.tag) {
    case "WebInit":
	launchChart(obj.contents);
	break;
    case "WebRender":
	updateChart(obj.contents)
	break;
	
    }
}

var chart = undefined;
var data = [];

function mkSeries(obj)
{
    let ss = obj.series;
    let sers = [];
    
    for (let k = 0; k < ss.length; k++) {
	data[k] = [];
	for(let i = 0; i < ss[k].seriesPoints.length; i++) {
	    data[k][i] = {x: ss[k].seriesPoints[i][0], y: ss[k].seriesPoints[i][1]};
	}
	
	let x = {
	    data: data[k].slice(),
	    color: ss[k].seriesColor,
	};
	sers.push(x);
    }

    //console.log(sers);
    
    return sers;
}

function launchChart(obj)
{
    //console.log(obj);
    
    let sers = mkSeries(obj);
    
    var options = {
        series: sers,
        chart: {
            id: 'realtime',
            height: 640,
            type: 'line',
            animations: {
		enabled: true,
		easing: 'linear',
		dynamicAnimation: {
		    speed: FRAMEPERIOD
		}
            },
            toolbar: {
		show: false
            },
          zoom: {
              enabled: false
          }
        },
        dataLabels: {
            enabled: false
        },
        stroke: {
            curve: 'straight'
        },
        title: {
            text: 'Популяция чибиков',
            align: 'center',
	    style: {
		fontSize:  '24px',
		fontWeight:  'bold',
		fontFamily:  'Victor Mono',
		color:  '#f3f3f3'
	    },
        },
	grid: {
	    xaxis: {
		lines: {
		    show: true,
		}
	    },

	    yaxis: {
		lines: {
		    show: true,
		}
	    },
	    
	    borderColor: '#f3f3f3'
	},
        markers: {
            size: 0
        },
        xaxis: {
	    type: 'numeric',
	    tickAmount: 'dataPoints',
            range: 1,
	    labels: {
		style: {
		    colors: "#f3f3f3",
		    fontSize: '16px',
		    fontFamily: 'Victor Mono',
		}
	    }
        },
        yaxis: {
	    tickAmount: 10,
	    min: -1,
            max: 1,
	    labels: {
		style: {
		    colors: "#f3f3f3",
		    fontSize: '16px',
		    fontFamily: 'Victor Mono',
		}
	    }
        },
        legend: {
            show: false
        },
    };
    
    const chel = document.getElementById('chart');
    
    chart = new ApexCharts(chel, options);
    chart.render();
          
    window.setInterval(frameCallback, FRAMEPERIOD);   

}

async function updateChart(obj)
{
    let sers = mkSeries(obj);
    
    chart.updateSeries(sers);

}

async function frameCallback()
{
    if(connectionEstablished) {
	
	let msg = "Step"; //{tag: "Step"};
    
	await sendMsg(msg);
    }
    
}

async function onLoad()
{
    console.log("8885");
   
}
