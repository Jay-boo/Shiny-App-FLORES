svg.selectAll("text").remove();
svg.selectAll("g").remove();
svg.selectAll("rect").remove();


margin = {top: height*50/400, right: 40, bottom: 30, left: 35*width/400};


const HOVERCOLOR="#62b1e1";
var background_c=options.background_color;
var title=options.title;
var var_name=options.var_name;
var y_lab=options.y_lab;/*ylab var*/
var short_var_name=options.short_var_name;/*ce qu'il y a au dessus des barres*/
var year=options.year
data.forEach(function(d){
	d.country=d.country;
	d.value=parseInt(d.value);
})

x = d3.scaleBand()
  .range([margin.left, width - margin.right])
  .domain(data.map(function(d) { return d.country; }))
  .padding(0.2);

y = d3.scaleLinear()
  .domain([0,d3.max(data.map(d=>d.value))])
  .range([height-margin.bottom,margin.top]);


svg.attr("width",width)
  .attr("height",height)
  .style("background-color", background_c);


const xAxis=svg.append("g")
  .call(d3.axisBottom(x))
  .attr("transform", "translate(0,"+(height-margin.bottom)+")");



var size_bar_lab=width*(15/500)*0.5;
var size_nb_etab=width*13/500;
var ticks_lab_y=0.7*margin.left*13/40;


xAxis.selectAll(".tick text")
  .attr("color","black")
  .attr("font-size","10px")
  .attr("font-weight","bold");

const yAxis=svg.append("g")
  .call(d3.axisLeft(y).ticks(null,"s"))
  .attr("transform","translate("+margin.left+",0)");


yAxis.append("text")
  .text(y_lab)
  .attr("x",3)
  .attr("y", margin.top-3)
  .attr("text-anchor","start")
  .attr("fill","grey")
  .attr("font-size","10px")
  .attr("font-weight","bold");

yAxis.selectAll(".tick text")
 .attr("color","black")
 .attr("font-size","10px")
 .attr("font-weight","bold");

svg.selectAll("myBar")
 .data(data)
 .enter()
 .append("rect")
   .attr("x",d=>x(d.country))
   .attr("width",x.bandwidth())
   .attr("fill","#6dc5fb")
   .attr("height",d=>height-margin.bottom-y(0))
   .attr("y",y(0))
   .append("title").text(d=>`${d.country}s :${d.value} ${short_var_name} en `+year);



svg.selectAll("rect")
 .transition()
 .duration(3000)
 .attr("y",d=>y(d.value))
 .attr("height",d=>height-margin.bottom-y(d.value))
 .delay(function(d,i){return(i*100)});

svg.selectAll("myLab")
 .data(data)
 .enter()
 .append("text")
   .attr("class","labs")
   .text(d=>`${d.value} ${short_var_name}`)//
   .attr("x",d=>x(d.country)+x.bandwidth()/2)
   .attr("y",y(0))
   .attr("fill","#8e9093")
   .attr("font-family","sans-serif")
   .attr("font-size","10px")
   .attr("font-weight","bold")
   .attr("text-anchor","middle");

   var size_title_text_margin_top=15*margin.top/50;
   var size_title_width=15*width/400;   
   var size_title_txt=d3.min([size_title_width,size_title_text_margin_top]);

svg.append("text")
    .text(title)//
    .attr("x",(width+margin.left)/2)
    .attr("y", margin.top/2)
    .attr("fill","black")
    .attr("font-family","sans-serif")
    .attr("font-size",size_title_txt+"px")
    .attr("font-weight","bold")
    .attr("text-anchor","middle");

svg.selectAll(".labs")
    .transition()
    .duration(3000)
    .attr("y",d=>y(d.value))
    .delay(function(d,i){return(i*100)});
    
      

svg.selectAll("rect")
    .on("mouseover",(event,v)=>{
      
      d3.select(event.currentTarget).attr("stroke-width",2).attr('stroke', `${background_c}`).attr("fill","#62b1e1");
    })
    .on("mouseout",(event,v)=>{
      
      d3.select(event.currentTarget).attr('stroke', 'None').attr("fill","#6dc5fb");
    })
  
     
