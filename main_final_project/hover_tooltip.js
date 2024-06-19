function(el) {
    var tooltip = d3.select('#' + el.id + ' .svg-container')
      .append("div")
      .attr("class", "my-custom-tooltip");

    el.on('plotly_hover', function(d) {
      var pt = d.points[0];
      // Choose a location (on the data scale) to place the image
      // Here I'm picking the top-left corner of the graph
      var x = pt.xaxis.range[0];
      var y = pt.yaxis.range[1];
      // Transform the data scale to the pixel scale
      var xPixel = pt.xaxis.l2p(x) + pt.xaxis._offset;
      var yPixel = pt.yaxis.l2p(y) + pt.yaxis._offset;

      // str to json
      var json_data = JSON.parse(pt.customdata);
      
      var img = "<img src='" +  json_data.url + "' width=100><br><div style='text-align: center;'>" + json_data.name + "</div>";
      tooltip.html(img)
        .style("position", "absolute")
        .style("left", xPixel*1.2 + "px")
        .style("top", yPixel*1.2 + "px");
      // Fade in the image
      tooltip.transition()
        .duration(300)
        .style("opacity", 1);
    });

    el.on('plotly_unhover', function(d) {
      // Fade out the image
      tooltip.transition()
        .duration(500)
        .style("opacity", 0);
    });
}