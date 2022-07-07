svg.selectAll("text").remove();
svg.selectAll("g").remove();

var background_c=options.background_color;
var title=options.title;

margin=({top:height*20/400,
       right:20*width/400,
       bottom:10,
       left:20*width/400});

var innerRadius=Math.min(width-margin.right-margin.left,height-margin.left-margin.right)/7;
var outerRadius=Math.min(width-d3.min([margin.right,margin.left]),height-d3.min([margin.left,margin.right]))/3;




gapminder_2005=data;

gapminder_2005_sorted = gapminder_2005.sort((a, b) => d3.descending(a.value, b.value));

xScale_sorted = d3.scaleBand()
  .domain(d3.map(gapminder_2005_sorted, d => d.country))
  .range([0, 2 * Math.PI])

yScale = d3.scaleRadial()
  .domain([0, d3.max(gapminder_2005, d => d.value)])
  .range([innerRadius, outerRadius])




//---------------------------------------------
// Starting the SVG element

svg.attr("height", height)
    .attr("width", width)
    .style("background-color", background_c);

var title_font_size=width*20/500;
var labs_size_width=(width-(margin.left+margin.right))*17/500;

svg.append("text")
  .attr("transform", "translate(" + (margin.left +(width-margin.left)/2 )+ "," + margin.top + ")")
  .attr("text-anchor","middle")
  .attr("fill", "maroon")
  .attr("font-size",title_font_size+"px")
  .style("font-weight", "bold")
  .style("font-family", "sans-serif")
  .text("Repartition Homme/Femme - Nombre d'emploi");


  // add the bars
svg.append("g")
  .attr("class","bars")
  .attr("transform", "translate(" + (margin.left+width/2) + "," + (height/2+margin.top) + ")")
  .selectAll("path")
  .data(gapminder_2005_sorted)
  .enter()
  .append("path")
  .attr("fill", "#6dc5fb")
  .attr("d", d3.arc()
       .innerRadius(innerRadius)
       .outerRadius(d => yScale(d.value))
       .startAngle(d => xScale_sorted(d.country))
       .endAngle(d => xScale_sorted(d.country) + xScale_sorted.bandwidth())
       .padAngle(0.01)
       .padRadius(innerRadius))
  .append("title")
    .text(d=>`${d.country=="Hommes" ? "H": "F"} : ${d.value} `);

  // add the labels
svg.selectAll("labs")
  .data(gapminder_2005_sorted)
  .enter()
    .append("text")
    .text(d => `${d.country} : ${d.value}`)
    .attr("x",d=> margin.left+width/2+ ((d.country=="Hommes" ? -1:1)*outerRadius))
    .attr("y",(height/2+margin.top))
    .attr("text-anchor","middle")
    .style("font-family", "sans-serif")
    .attr("font-weight","bold")
    .attr("font-size",labs_size_width+"px");
    


svg.selectAll("path")
  .on("mouseover",(event,v)=>{
    
    d3.select(event.currentTarget).attr("stroke-width",2).attr('stroke', 'white').attr("fill","#62b1e1");
  })
  .on("mouseout",(event,v)=>{
    
    d3.select(event.currentTarget).attr('stroke', 'None').attr("fill","#6dc5fb");
  })

