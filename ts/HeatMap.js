"use strict";
//Object.defineProperty(exports, "__esModule", { value: true });
const Data_1 = require("./Data");
const d3 = require("d3");
require("plotly");
class HeatMap {
    constructor() {
        this.colors = new Array();
        this.data = new Data_1.default("heatmap", [[1, 20, 30], [20, 1, 60], [30, 60, 1]]);
    }
    colorScale(temp) {
        let lower;
        let upper;
        let ranges = new Array();
        ranges = [0, 3, 5.5, 6, 6.5, 7, 8.5, 9, 9.5, 10, 15];
        for (let i in ranges) {
            let j = Number(i);
            lower = ranges[j];
            upper = ranges[j + 1];
            if (temp >= lower && temp < upper) {
                return this.colors[j];
            }
        }
    }
    makePlot(temp) {
        let margin = { top: 75, right: 15, bottom: 125, left: 85 };
        let width = 1200 - margin.left - margin.right;
        let height = 555 - margin.top - margin.bottom;
        let x = d3.scale.linear().range([0, width]);
        let y = d3.scale.linear().range([0, height]);
        let mint = "#05B89A";
        let teal = "#0B90B6";
        let corn = "#FFCE65";
        let yellow = "#FFC039";
        let gold = "#FFC707";
        let orange = "#FF9339";
        let sun = "#FF6307";
        let heart = "#FF4739";
        let red = "#FF1907";
        let hot = "960018";
        this.colors = [mint, teal, corn, yellow, gold, orange, sun, heart, red, hot];
        let legendScale;
        legendScale = ["0 - 3",
            "3 - 5.5", "5.5 - 6", "6 - 6.5", "6.5 - 7", "7 - 8.5", "8.5 - 9", "9 - 9.5", "9.5 - 10", "10+"];
        let svg = d3.select("#chart").append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");
        let div = d3.select("body").append("div")
            .attr("class", "tooltip")
            .style("opacity", 0);
        let color = this.colorScale(temp);
        return (color);
    }
}
exports.default = HeatMap;
//# sourceMappingURL=HeatMap.js.map