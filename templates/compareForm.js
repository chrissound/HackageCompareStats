$(function () {
    var jsonData = {{{ jsonData }}};
    Highcharts.chart('container', {
        chart: {
            type: 'column'
        },
        title: {
            text: 'Packages downloads'
        },
        yAxis: {
            min: 0,
            title: {
                text: 'Download count',
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
    });
});
