svg.selectAll("text").remove();
svg.selectAll("g").remove();


const margin={top:10,
    right:10,
    bottom:10,
    left:10};

const innerRadius=150;
const outerRadius=Math.min(width,height)/2;
var background_c=options.background_color;
var title=options.title;

gapminder_2005=data;

gapminder_2005_sorted = gapminder_2005.sort((a, b) => d3.descending(a.value, b.value));

xScale = d3.scaleBand()
  .domain(d3.map(gapminder_2005, d => d.country))
  .range([0, 2 * Math.PI]);

yScale = d3.scaleRadial()
  .domain([0, d3.max(gapminder_2005, d => d.value)])
  .range([innerRadius, outerRadius]);



xScale_sorted = d3.scaleBand()
  .domain(d3.map(gapminder_2005_sorted, d => d.country))
  .range([0, 2 * Math.PI])


//---------------------------------------------
// Starting the SVG element

svg.attr("height", height)
    .attr("width", width)
    .style("background-color", background_c);


svg.append("text")
    .attr("transform", "translate(" + 20 + "," + 20 + ")")
    .attr("fill", "#0c4493")
    .style("font-weight", "bold")
    .style("font-family", "Helvetica Neue, Arial")
    .text(title);

  // add the bars
svg.append("g")
    .attr("transform", "translate(" + width/2 + "," + (height/2 + 50) + ")")
    .selectAll("path")
    .data(gapminder_2005_sorted)
    .enter()
    .append("path")
    .attr("fill", "#5b89cc")
    .attr("d", d3.arc()
         .innerRadius(innerRadius)
         .outerRadius(d => yScale(d.value))
         .startAngle(d => xScale_sorted(d.country))
         .endAngle(d => xScale_sorted(d.country) + xScale_sorted.bandwidth())
         .padAngle(0.01)
         .padRadius(innerRadius));

  // add the labels
svg.append("g")
    .attr("transform", "translate(" + width/2 + "," + (height/2 + 50) + ")")
    .selectAll("g")
    .data(gapminder_2005_sorted)
    .enter()
    .append("g")
    .attr("text-anchor", d => ((xScale_sorted(d.country) + xScale_sorted.bandwidth() / 2 + Math.PI) % (2 * Math.PI) < Math.PI ? "end" : "start"))   // didnt quite understand the purpose of the calculation here :(
    .attr("transform", d => ("rotate(" + ((xScale_sorted(d.country) + xScale_sorted.bandwidth() / 2) * 180 / Math.PI - 90) + ")" + "translate(" + (yScale(d.value) + 10) + ",0)"))    // -90 in calculating the rotation angle is because by default, text is horizontal, which is already the 90° direction; +10 is to add a bit space between the bar and the text
    .append("text")
    .text(d => `${d.country} - ${d.value}`)
    .attr("transform", d => ((xScale_sorted(d.country) + xScale_sorted.bandwidth() / 2 + Math.PI) % (2 * Math.PI) < Math.PI ? "rotate(180)" : "rotate(0)" ))      // rotate the direction of text; see: Arabia vs China at 12 o clock
    .style("font-size", "11px")
    .attr("alignment-baseline", "middle")


