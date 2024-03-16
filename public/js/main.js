
document.addEventListener("DOMContentLoaded", onLoad);

const n = 16;
const dt = 1.0 / 16.0;
var t = 0;
var data = [];


function calcData(t0)
{
    data = [];
    
    let dt = 1.0 / n;
    let x = t0;
    
    for(let i = 0; i < n; i++) {
	let y = Math.sin(2 * Math.PI * x);
	data.push({x: x, y: y});
	x = x + dt;
    }
}



function onLoad()
{
    console.log("8885");

    calcData(t);
    
    var options = {
        series: [{
            data: data.slice()
        }],
        chart: {
            id: 'realtime',
            height: 640,
            type: 'line',
            animations: {
		enabled: true,
		easing: 'linear',
		dynamicAnimation: {
		    speed: 1000
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
            curve: 'smooth'
        },
        title: {
            text: 'Dynamic Updating Chart',
            align: 'left'
        },
        markers: {
            size: 0
        },
        xaxis: {
	    type: 'numeric',
	    tickAmount: 'dataPoints',
            range: 1,
        },
        yaxis: {
	    min: -1,
            max: 1
        },
        legend: {
            show: false
        },
    };

    
    const chel = document.getElementById('chart');
    
    var chart = new ApexCharts(chel, options);
    chart.render();
      
    
    window.setInterval(function () {
	
	t = t + dt;
	calcData(t);
	
        chart.updateSeries([{
          data: data
        }])
    }, 1000)   
   
}
