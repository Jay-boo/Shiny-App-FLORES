
svg.selectAll("text").remove();
svg.selectAll("g").remove();
svg.selectAll("circle").remove();
const HOVERCOLOR="#62b1e1";


    svg.attr("width", width)          // change the attributes
    .attr("height", height)
	.style("background_color","red");
  
  svg.append("circle")         // add a circle nested in the svg element
    .attr("fill", "blue")       // change the attributes
    .attr("cx", 24)
    .attr("cy", 24)  
    .attr("r", 24); 
  
  const i = svg.append("g");    // add a g nested in the svg element, after the circle
                                // and refer to this element as "i" (this is NOT a copy, but rather an alias)
  
  i.append("circle")            // add a circle nested in the g element aliased as "i"
    .attr("fill", "white")      // change the attributes
    .attr("cx", 24)
    .attr("cy", 11.6)  
    .attr("r", 4.7);
  
  i.append("path")              // add a path nested in the g element aliased as "i"
    .attr("fill", "white")
    .attr("d", "m17.4 18.8v2.15h1.13c2.26 0 2.26 1.38 2.26 1.38v15.1s0 1.38-2.26 1.38h-1.13v2.08h14.2v-2.08h-1.13c-2.26 0-2.26-1.38-2.26-1.38v-18.6");


    svg.append("text")
      .text("Pas de données disponibles pour les EPCI sur cette variable")
      .attr("x",24+24+5)
      .attr("y", 24)
      .attr("fill","black")
      .attr("font-size","15px")
      .attr("font-weight","bold")
      .attr("text-anchor","start");

