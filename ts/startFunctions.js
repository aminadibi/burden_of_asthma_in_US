

shinyjs.backgroundCol = function(params) {
  var defaultParams = {
    id : null,
    col : "red"
  };
  params = shinyjs.getParams(params, defaultParams);

  var el = $("#" + params.id);
  el.css("background-color", params.col);
};

shinyjs.trying = trying;
function trying(params) {
  alert("trying");
}

"use strict";
//Object.defineProperty(exports, "__esModule", { value: true });
//require("./HeatMap");
require("./Data");
function startHeatMap(temp) {
  alert("heatmap");
console.log("heatmap");
    //let heatMap = new HeatMap_1.default();
    //let color = heatMap.makePlot(temp);
    return "blue";
}
shinyjs.startHeatMap = startHeatMap;
//class Data {
    //constructor(type, z) {
        //this.z = [];
        //this.type = "heatmap";
        //this.type = type;
        //this.z = z;
    //}
//}
//# sourceMappingURL=startFunctions.js.map
