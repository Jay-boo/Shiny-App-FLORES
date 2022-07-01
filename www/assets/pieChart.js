



var id_container=options.id_container;
var background_c=options.background_color;

var min_dim= Math.min(width,height);
var radius=(min_dim/2)-0.1* min_dim;
const padding=5;

pie = d3.pie()
    .value(d => d.nb_etab);

path= d3.arc()
    .outerRadius(radius - padding)
    .innerRadius((radius - padding) / 2);

color = d3.scaleOrdinal()
    .domain(data)
    .range(d3.schemePastel1);



function calcTranslate(data, move = 4) {
    const moveAngle = data.startAngle + ((data.endAngle - data.startAngle) / 2);
    return `translate(${- move * Math.cos(moveAngle + Math.PI / 2)}, ${- move * Math.sin(moveAngle + Math.PI / 2)})`;
}
label = d3.arc()
    .outerRadius(radius - padding)
    .innerRadius((radius - padding) / 2)

const textThreshold = Math.PI / 8
//--------------------------------------------------
// Starting the svg element

svg.attr("width",width)
    .attr("height", height)
    .style("background-color", background_c);
const g =svg.append("g")
    .datum(data)
    .style('font-family', 'sans-serif')
  	.attr('transform', `translate(${ width / 2}, ${ height / 2})`);

const duration =250;

const arc = g.selectAll('.arc')
              .data(d => pie(d))
              .enter()
              .append('g')
              .attr('class', '.arc')
				.style('cursor', 'pointer')
                .on('mouseover', (event, v) => {
    				d3.select(event.currentTarget)
                      	.transition()
                  	  	.duration(duration)
                  		.attr('transform', calcTranslate(v, 6));
                  	d3.select(event.currentTarget).select('path')
                  		.transition()
                  		.duration(duration)
                  		.attr('stroke', 'rgba(100, 100, 100, 0.2)')
                  		.attr('stroke-width', 4);
                  	d3.select('.card-back text').text(v.data.REG);
  				})
        .on('mouseout', (event, v) => {
                  d3.select(event.currentTarget)
                  	.transition()
                  	.duration(duration)
                  	.attr('transform', 'translate(0, 0)');
                  d3.select(event.currentTarget).select('path')
                  		.transition()
                  		.duration(duration)
                  		.attr('stroke', 'white')
                  		.attr('stroke-width', 1);
                });

arc.append("path")
  .attr("d",path)
  .attr("fill",function(d){return color(d.data.REG)})
  .attr('stroke', 'white');



const labels=arc.append("g")
  .attr("transform",v=> `translate(${label.centroid(v)})`)
  .attr('text-anchor', 'middle')
    .style('fill', 'black')
    .style('font-size', '75%')
    .style('display', v => v.endAngle - v.startAngle > textThreshold ? 'inline' : 'none');

labels.append('text')
      .text(v => v.data.REG);

labels.append('text')
        .attr('dy', '1.2em')
        .style('font-size', '90%')
        .text(v => ` ${v.data.nb_etab}`);
 