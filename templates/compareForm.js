$(function () {
    var jsonData = {{{ jsonData }}};
    Highcharts.chart('container', {
        chart: {
            type: 'column'
        },
        title: {
            text: 'Packages popularity'
        },
        yAxis: {
            min: 0,
            title: {
                text: 'Popularity index',
                align: 'high'
            },
            labels: {
                overflow: 'justify'
            }
        },
        plotOptions: {
            column: {
                pointPadding: 0,
                borderWidth: 0,
                groupPadding: 0,
                shadow: false
            }
        },
        legend: {
            layout: 'vertical',
            align: 'right',
            verticalAlign: 'top',
            x: -40,
            y: 80,
            floating: true,
            borderWidth: 1,
            backgroundColor: ((Highcharts.theme && Highcharts.theme.legendBackgroundColor) || '#FFFFFF'),
            shadow: true
        },
        credits: {
            enabled: false
        },
        series : jsonData.series
        // series: [{
        //     name: 'Year 1800',
        //     data: [107, 31, 635, 203, 2]
        // }, {
        //     name: 'Year 1900',
        //     data: [133, 156, 947, 408, 6]
        // }, {
        //     name: 'Year 2012',
        //     data: [1052, 954, 4250, 740, 38]
        // }]
    });
});
