const DISPUTANT_COLUMN = 0;
const YEAR_COLUMN = 1;
const TOTAL_DISPUTES_COLUMN = 2;
const RADIUS = 10;
const MIN_WIDTH = 2*RADIUS;
const MAX_WIDTH = 250;
const FONT_SIZE = 20;
const HEIGHT = FONT_SIZE*2.5;
const THICKNESS = 2*RADIUS;
const MAX_BRIGHTNESS=0.95;
const RESOLUTION=256;
const YEAR_WIDTH = 200;

class Point2D
{
	constructor( x , y )
	{
		this.x = x;
		this.y = y;
	}
	length(){ return Math.sqrt( this.x*this.x + this.y*this.y); }
	perp(){ return new Point2D( this.y , -this.x ); }
	dot( p ){ return this.x*p.x + this.y*p.y; }
	add( p ){ return new Point2D( this.x + p.x , this.y + p.y ); }
	subtract( p ){ return new Point2D( this.x - p.x , this.y - p.y ); }
	scale( s ){ return new Point2D( this.x*s , this.y*s ); }
	
	toString()
	{
		return "( " + this.x + " , " + this.y + " )";
	}
}

class Circle
{
	constructor( c , r )
	{
		this.c = c;
		this.r = r;
	}
}

class DisputantInfo
{
	constructor( name , totalDisputes , year )
	{
		this.name = name;
		this.totalDisputes = parseInt( totalDisputes );
		if( year.length>0 )
			this.year = parseInt( year );
		else
		{
			this.year = Infinity;
		}
		this.position = new Point2D(0,0);
	}
	hasYear()
	{
		return this.year != Infinity;
	}
	toString()
	{
		return "( " + this.name + " , " + this.totalDisputes + " , " + this.year + " )";
	}

};

class DisputeInfo
{
	constructor( id1 , id2 , count1 , count2 )
	{
		this.id1 = id1;
		this.id2 = id2;
		this.count1 = parseInt( count1 );
		this.count2 = parseInt( count2 );
		this.perpOffset = 0;
	}
	toString()
	{
		return "( " + this.id1 + " , " + this.id2 + " , " + this.count1 + " , " + this.count2 + " )";
	}
};
var yearStart;
var yearEnd;
var steps;
var disputeData = [];	// An array of DisputeInfo's
var disputantData = [];	// An array of DisputantInfo's
var minYear;
var maxYear;
var maxTotalDisputes;
var canvas;
var selectedID = -1;
var isDispute = false;
var mouseX;
var mouseY;
var maxCount;

var textFile = null;

function getCircle( p1 , p2 , p3 )
{
	// Edge mid-points
	var p12 = new Point2D( (p1.x+p2.x)/2 , (p1.y+p2.y)/2 );
	var p23 = new Point2D( (p2.x+p3.x)/2 , (p2.y+p3.y)/2 );

	// Edge directions
	var d12 = new Point2D( p2.x-p1.x , p2.y-p1.y );
	var d23 = new Point2D( p3.x-p2.x , p3.y-p2.y );
	
	// Edge perpendicular directions
	var _d12 = d12.perp();
	var _d23 = d23.perp();
	
	// solve for p12 + _d12 *s = p23 + _d23 * t
	// _d12 * s - _d23 * t = -p12 + p23
	var a00 = _d12.x , a01 = _d12.y;
	var a10 = -_d23.x , a11 = -_d23.y;
	var b0 = -p12.x+p23.x , b1 = -p12.y+p23.y;
	var det = a00 * a11 - a01 * a10;
	var x0 = (   b0 * a11 - b1 * a10 ) / det;
	var x1 = ( - b0 * a01 + b1 * a00 ) / det;
	
	var center = new Point2D( p12.x + _d12.x * x0 , p12.y + _d12.y * x0 );
	return new Circle( center , ( p1.subtract( center ).length() + p2.subtract( center ).length() + p3.subtract( center ).length() ) / 3. );
}

function makeTextFile(text)
{
	var data = new Blob([text], {type: 'text/plain'});

	// If we are replacing a previously generated file we need to
	// manually revoke the object URL to avoid memory leaks.
	if (textFile !== null) window.URL.revokeObjectURL(textFile);

	textFile = window.URL.createObjectURL(data);

	return textFile;
};

function createDownloadLink()
{
	var link = document.getElementById('downloadlink');
	var text_to_save = "";
	// Set text_to_save to the disputant state
	text_to_save += disputantData.length + "\n";
	for (var i = 0; i<disputantData.length; i++) {
		text_to_save += disputantData[i].name + "," + disputantData[i].position.x + "," + disputantData[i].position.y + "\n";
	}
	text_to_save += disputeData.length + "\n";
	for (var i = 0; i<disputeData.length; i++) {
		text_to_save += disputeData[i].id1 + "," + disputeData[i].id2 + ", " + disputeData[i].perpOffset +  "\n";
	}

	link.href = makeTextFile( text_to_save );
	link.style.display = 'block'; 
}
function parseUpload( str )
{
	// Disputant indices
	var disputantIndices = [];
	
	// Separate the file into separate lines
	var lines = str.split( "\n" );
	
	// Get the number of disputants
	var disputantNum = parseInt( lines[0] );
	
	// Read disputant info
	for (var i = 0; i < disputantNum; i++) {
		var values = lines[i + 1].split( "," );
		if( values.length>1 ) {
			var idx = disputantIndex(values[0]);
			disputantIndices[i] = idx;
			if (idx != -1) {
				disputantData[idx].position.x = Math.floor(values[1]);
				disputantData[idx].position.y = Math.floor(values[2]);
			}
		}
	}
	// Get the number of disputes
	var disputeNum = parseInt( lines[disputantNum + 1] );
	
	// Read dispute info
	for (var i = 0; i < disputeNum; i++) {
		var values = lines[i + disputantNum + 2].split( "," );
		var id1 = disputantIndices[ parseInt( values[0] ) ];
		var id2 = disputantIndices[ parseInt( values[1] ) ];
		var offset = parseFloat( values[2] );
		if (id1 != -1 && id2!= -1) {
			for (var n = 0; n < disputeData.length; n++) {
				if (disputeData[n].id1 == id1 && disputeData[n].id2 == id2) {
					disputeData[n].perpOffset = offset;
				}
				else if (disputeData[n].id1 == id2 && disputeData[n].id2 == id1) {
					disputeData[n].perpOffset = -offset;
				}
			}
		}
	}
	draw();
}
function uploadFile(info) {

	if (disputantData.length != 0) {
		var fr=new FileReader();
		var file= info.files[0];
		fr.onload = function(){ parseUpload(fr.result); };
		fr.readAsText(file);

		/*var foo = document.getElementById("disputes");
		foo.innerHTML = "Disputes file: <input type=\"file\" id=\"disputebutton\" onChange=\"loadDisputes(this)\">";
		document.getElementById("disputebutton").click();
		*/
	}
}
function parseDisputesFileContents( str )
{
	var _disputeData = [];
	for( var i=0 ; i<disputantData.length ; i++ ) _disputeData[i] = [];
	var lines = str.split( "\r\n" );
	var names = lines[0].split( "," );
	for( var i=1 ; i<lines.length; i++) {
		var values=lines[i].split( "," );
		if( values.length>1 ) {
			var index1 = disputantIndex(values[0]);
			if (index1 != -1) {
				for (var j = 1; j<values.length; j++) {
					var index2 = disputantIndex(names[j]);
					if (index2 != -1) {
						_disputeData[index1][index2] = values[j];
					}
				}
			}
		}
	}
	
	disputeData = [];
	for( var i=0 ; i<_disputeData.length ; i++ ) for( var j=0 ; j<i ; j++ )
	{
		if( _disputeData[i][j]!=0 || _disputeData[j][i]!=0 )
		{
			var dispute = new DisputeInfo( i , j , _disputeData[i][j] , _disputeData[j][i] );
			disputeData.push( dispute );
		}
	}
	
	maxCount = 0;
	for (var i = 0; i<disputeData.length; i++) {
		if (disputeData[i].count1 > maxCount) {
			maxCount = disputeData[i].count1;
		}
		if (disputeData[i].count2 > maxCount) {
			maxCount = disputeData[i].count2;
		}
	}
	disputeData.sort( function(a,b){ return (a.count1+a.count2) - (b.count1+b.count2); } );

	draw();
}
function loadDisputes( input )
{
	var fr=new FileReader();
	var file=input.files[0];
	fr.onload = function(){ parseDisputesFileContents(fr.result); start();};
	fr.readAsText(file);
	var foo = document.getElementById("upload");
	foo.innerHTML = "Upload file: <input type=\"file\" onChange=\"uploadFile(this)\">";
	start();
}

function disputantIndex(name) {
	for (var i=0; i<disputantData.length; i++) {
		if (disputantData[i].name == name) return i;
	}
	return -1;
}
function parseDisputantsFileContents( str )
{
	var lines = str.split( "\n" );
	for( var i=0 ; i<lines.length; i++) {
		var values = lines[i].split( "," );
		if( values.length>1 )
			disputantData.push( new DisputantInfo( values[DISPUTANT_COLUMN] , values[TOTAL_DISPUTES_COLUMN] , values[YEAR_COLUMN] ) );
	}
	disputantData.sort( function(a,b){ return b.totalDisputes - a.totalDisputes; } );
}
function loadDisputants( input )
{
	var fr=new FileReader();
	var file=input.files[0];
	fr.onload = function(){ parseDisputantsFileContents(fr.result); };
	fr.readAsText(file);

	var foo = document.getElementById("disputes");
	foo.innerHTML = "Disputes file: <input type=\"file\" id=\"disputebutton\" onChange=\"loadDisputes(this)\">";
	document.getElementById("disputebutton").click();
}

function yearToFraction(year){
	if (year==Infinity) return 0.5;
	else return 1-(year-minYear)/(maxYear-minYear);
}

function fractionToY( fraction )
{
	return ( canvas.height - HEIGHT ) * fraction;
//	return canvas.height * fraction;
}

function draw () {
	var ctx = canvas.getContext("2d");
	ctx.clearRect(0, 0, canvas.width, canvas.height);
	drawYears();

	for (var i=0; i<disputeData.length; i++) {
		drawDispute(i);
	}

	for (var i=0; i<disputantData.length; i++) {
		drawDisputant(i);
	}

}
function drawYears() {
	var ctx = canvas.getContext("2d");
	yearStart = (Math.floor(minYear/50)) * 50;
	yearEnd = (Math.ceil(maxYear/50)) * 50;
	steps = (yearEnd - yearStart) / 50;
	for (var i = 0; i < steps; i += 1) {
		ctx.font = "16px Arial";
		ctx.fillStyle = "#0095DD";
		ctx.textAlign = "center";
		//ctx.fillText(i + yearStart, 50, fractionToY( yearToFraction( i + yearStart ) ) );	
		var tHeight = yearToFraction( i * 50 + yearStart );
		ctx.fillText(i * 50 + yearStart, 50, fractionToY( tHeight ) );				
	}
}
function drawLine (x1, y1, x2, y2) {
	var ctx = canvas.getContext("2d");
	ctx.beginPath ();
	ctx.moveTo (x1, y1);
	ctx.lineTo (x2, y2);
	ctx.stroke ();
	ctx.closePath ();
}

function drawRoundedRectangle( center , width , height , radius , i)
{
	var ctx = canvas.getContext("2d");
	var stroke;
	
	// Draw rectangle interior
	if (disputantData[i].hasYear() )
	{
		stroke = "#ffffff";
	}
	else {
		stroke = "#ffe0e0";	
	}
	ctx.fillStyle = stroke;
	
	ctx.beginPath ();
	ctx.arc (center.x + width/2 - radius, center.y + height/2 - radius, radius, 0, Math.PI/2);
	ctx.lineTo( center.x + width/2 - radius, center.y + height/2 - radius );
	ctx.fill ();
	
	ctx.beginPath ();
	ctx.arc (center.x + width/2 - radius, center.y - height/2 + radius, radius, -Math.PI/2, 0);
	ctx.lineTo( center.x + width/2 - radius, center.y - height/2 + radius );
	ctx.fill ();
	
	ctx.beginPath ();
	ctx.arc (center.x - width/2 + radius, center.y + height/2 - radius, radius, Math.PI/2, Math.PI);
	ctx.lineTo( center.x - width/2 + radius, center.y + height/2 - radius );
	ctx.fill ();
	
	ctx.beginPath ();
	ctx.arc (center.x - width/2 + radius, center.y - height/2 + radius, radius, -Math.PI, -Math.PI/2);
	ctx.lineTo( center.x - width/2 + radius, center.y - height/2 + radius );
	ctx.fill ();
	
	ctx.beginPath();
    ctx.rect(center.x - width/2, center.y - height/2 + radius, width, height - radius*2);
	ctx.rect(center.x - width/2 + radius, center.y - height/2, width - radius*2, height);
    ctx.fill();
    ctx.closePath();
	
	// Draw rectangle boundary
	ctx.strokeStyle = "#000000";
	drawLine (center.x - width/2, center.y - height/2 + radius, center.x - width/2, center.y + height/2 - radius);
	drawLine (center.x - width/2 + radius, center.y + height/2, center.x + width/2 - radius, center.y + height/2);
	drawLine (center.x + width/2, center.y + height/2 - radius, center.x + width/2, center.y - height/2 + radius);
	drawLine (center.x + width/2 -radius, center.y - height/2, center.x - width/2 + radius, center.y - height/2);
	
	
	ctx.beginPath ();
	ctx.arc (center.x + width/2 - radius, center.y + height/2 - radius, radius, 0, Math.PI/2);
	ctx.stroke ();
	
	ctx.beginPath ();
	ctx.arc (center.x + width/2 - radius, center.y - height/2 + radius, radius, -Math.PI/2, 0);
	ctx.stroke ();
	
	ctx.beginPath ();
	ctx.arc (center.x - width/2 + radius, center.y + height/2 - radius, radius, Math.PI/2, Math.PI);
	ctx.stroke ();
	
	ctx.beginPath ();
	ctx.arc (center.x - width/2 + radius, center.y - height/2 + radius, radius, -Math.PI, -Math.PI/2);
	ctx.stroke ();
}

function drawDisputant(id) {
	var ctx = canvas.getContext("2d");
	drawRoundedRectangle( disputantData[id].position, setMaxWidth(id), HEIGHT, RADIUS , id);
		

	ctx.font = "" + FONT_SIZE + "px Arial";
    ctx.fillStyle = "#0095DD";
	ctx.textAlign = "center";
	var width = ctx.measureText(disputantData[id].name).width;
	if( width < setMaxWidth(id) )	{
		ctx.fillStyle = "#0095DD";
		ctx.fillText(disputantData[id].name, (disputantData[id].position.x), disputantData[id].position.y + FONT_SIZE/2);
		
	}
	else {
		ctx.fillStyle = "#000000";
		ctx.fillText(disputantData[id].name, (disputantData[id].position.x), disputantData[id].position.y + HEIGHT/2 + FONT_SIZE );
	}
}

function byteToHex( b )
{ 
  var hex = Number(b).toString(16);
  if (hex.length < 2) {
       hex = "0" + hex;
  }
  return hex;
};

function countToRGBString( count )
{
	var f = count / maxCount;
	var rg = Math.floor( 255 * ( 1- f ) );
	var b = 255;
	rg *= MAX_BRIGHTNESS;
//	b *= MAX_BRIGHTNESS;
	return "rgb(" + rg + "," + rg + "," + b + ")";
	
	/*
	f = 1 - f;
	f *= MAX_BRIGHTNESS;
	var b = Math.floor( f * 255 );
	return "rgb(255,255," + (255-b) + ")";
//	return "rgb(" + b + "," + b + "," + b + ")";
	*/
}
function inTrapezoid( p , p1 , p2 , thickness1 , thickness2 )
{
	var d = new Point2D( p2.x-p1.x , p2.y-p1.y );
	var length = d.length();
	d.x /= length;
	d.y /= length;
	var d_perp = d.perp();

	var _p = new Point2D( p.x - p1.x , p.y - p1.y );
	var d = _p.dot( d );
	var x = d/length;
	if( x<0 || x>1 ) return false;
	var y = _p.dot( d_perp );
	var thickness = thickness1 - x * thickness1 + x * thickness2;
	return Math.abs(y)<thickness;
}
function drawTrapezoid( p1 , p2 , thickness1 , thickness2 , count1 , count2 )
{
	var d = new Point2D( p2.x-p1.x , p2.y-p1.y );
	var length = d.length();
	d.x /= length;
	d.y /= length;
	var d_perp = d.perp();

	var ctx = canvas.getContext("2d");
	
	var my_gradient = ctx.createLinearGradient( p1.x , p1.y, p2.x , p2.y );
	var rgb1 = countToRGBString( count1 );
	var rgb2 = countToRGBString( count2 );
	my_gradient.addColorStop( 0 , rgb1 );
	my_gradient.addColorStop( 1 , rgb2 );
	ctx.fillStyle = my_gradient;
	ctx.beginPath ();
	ctx.moveTo (p1.x + thickness1*d_perp.x, p1.y + thickness1*d_perp.y);
	ctx.lineTo (p2.x + thickness2*d_perp.x, p2.y + thickness2*d_perp.y);
	ctx.lineTo (p2.x - thickness2*d_perp.x, p2.y - thickness2*d_perp.y);
	ctx.lineTo (p1.x - thickness1*d_perp.x, p1.y - thickness1*d_perp.y);
	ctx.lineTo (p1.x + thickness1*d_perp.x, p1.y + thickness1*d_perp.y);
	ctx.fill ();
	ctx.closePath ();
}

function isInArc( p , circle , p1 , p2 , thickness1 , thickness2 )
{
	var center_p = p.subtract( circle.c );
	var p1_p2 = p2.subtract( p1 );
	// Want to solve for s and t such that (p1.x,p1.y) + s * (p1_p2.x,p1_p2.y) = (center.x,center.y) + t * (center_p.x,center_p.y)
	// s * (p1_p2.x,p1_p2.y) - t * (center_p.x,center_p.y) = (center.x,center.y) - (p1.x,p1.y)
	var a00 = p1_p2.x;
	var a01 = p1_p2.y;
	var a10 = -center_p.x;
	var a11 = -center_p.y;
	var det = a00 * a11 - a10 * a01;
	var b1 = circle.c.x-p1.x;
	var b2 = circle.c.y-p1.y;
	if( Math.abs(det)<.00001 ) return false;
	var s = (  a11 * b1 - a10 * b2 ) / det;
	var t = ( -a01 * b1 + a00 * b2 ) / det;
	
	if( s<0 || s>1 || t<0 ) return false;
	var thickness = (1-s) * thickness1 + s * thickness2;
	var l = center_p.length();
	var radius = circle.r;
	return l>radius-thickness && l<radius+thickness;
}

function drawArc( center, p1 , p2 , thickness1 , thickness2 , count1 , count2 )
{
	var _p1 = new Point2D( p1.x-center.x , p1.y-center.y );
	var _p2 = new Point2D( p2.x-center.x , p2.y-center.y );
	var radius = _p1.length();
	
	var ctx = canvas.getContext("2d");

	var my_gradient = ctx.createLinearGradient( p1.x , p1.y, p2.x , p2.y );
	var rgb1 = countToRGBString( count1 );
	var rgb2 = countToRGBString( count2 );
	my_gradient.addColorStop( 0 , rgb1 );
	my_gradient.addColorStop( 1 , rgb2 );
	ctx.fillStyle = my_gradient;

	ctx.beginPath ();
	for( var i=0 ; i<=RESOLUTION ; i++ )
	{
		var s = i/RESOLUTION;
		var thickness = thickness1 - s * thickness1 + s * thickness2;
		var _p = new Point2D( _p1.x - s * _p1.x + s * _p2.x, _p1.y - s * _p1.y + s * _p2.y);
		_p = _p.scale( ( radius-thickness) / _p.length() );
		var p = new Point2D( _p.x + center.x , _p.y + center.y );
		if( i==0 ) ctx.moveTo( p.x , p.y );
		else       ctx.lineTo( p.x , p.y );
	}
	for( var i=RESOLUTION ; i>=0 ; i-- )
	{
		var s = i/RESOLUTION;
		var thickness = thickness1 - s * thickness1 + s * thickness2;
		var _p = new Point2D( _p1.x - s * _p1.x + s * _p2.x, _p1.y - s * _p1.y + s * _p2.y);
		_p = _p.scale( ( radius+thickness) / _p.length() );
		var p = new Point2D( _p.x + center.x , _p.y + center.y );
		ctx.lineTo( p.x , p.y );
	}
	ctx.fill ();
	ctx.closePath ();
}

function drawDispute(id)
{
	var ctx = canvas.getContext("2d");
	ctx.strokeStyle = "#000000";
	var id1 = disputeData[id].id1;
	var id2 = disputeData[id].id2;

	if( disputeData[id].perpOffset!=0 )
	{
		var id1 = disputeData[id].id1;
		var id2 = disputeData[id].id2;
		var p1 = disputantData[id1].position;
		var p2 = disputantData[id2].position;
		var p12 = new Point2D( (p1.x+p2.x)/2 , (p1.y+p2.y)/2 );
		var d12 = p2.subtract(p1).perp();
		d12 = d12.scale( disputeData[id].perpOffset / d12.length() );
		var p = p12.add( d12 );
		var circle = getCircle( disputantData[id1].position , disputantData[id2].position , p );
		
		var c1 = disputeData[id].count1;
		var c2 = disputeData[id].count2;
		
		var t1 = THICKNESS*disputeData[id].count1/maxCount;
		var t2 = THICKNESS*disputeData[id].count2/maxCount;
		drawArc( circle.c , disputantData[id1].position , p , t1 , (t1+t2)/2 , c1 , (c1+c2)/2 );
		drawArc( circle.c , p , disputantData[id2].position , (t1+t2)/2 , t2 , (c1+c2)/2 , c2 );
	}
	else
	{
		drawTrapezoid( disputantData[id1].position , disputantData[id2].position , THICKNESS*disputeData[id].count1/maxCount , THICKNESS*disputeData[id].count2/maxCount , disputeData[id].count1 , disputeData[id].count2 );
	}
}

function setMaxWidth (i)
{
	var x = disputantData[i].totalDisputes / maxTotalDisputes;
	var calculated = (MAX_WIDTH - MIN_WIDTH) * x + MIN_WIDTH;
	if ( calculated > MIN_WIDTH) {
		return calculated;
	}
	else return MIN_WIDTH;
}
function mouseDownHandler() {
	mouseX = event.x;
	mouseY = event.y;
	var x = event.x-canvas.offsetLeft;
	var y = event.y-canvas.offsetTop;

	selectedID = -1;
	
	var p = new Point2D( x , y );
	for (var c = 0; c<disputeData.length; c++) {
		var id1 = disputeData[c].id1;
		var id2 = disputeData[c].id2;

		if( disputeData[c].perpOffset==0 )
		{
			if( inTrapezoid( p , disputantData[id1].position , disputantData[id2].position , THICKNESS*disputeData[c].count1/maxCount , THICKNESS*disputeData[c].count2/maxCount)) {
				selectedID = c;
				isDispute = true;
			}
		}
		else
		{
			var p1 = disputantData[id1].position;
			var p2 = disputantData[id2].position;
			var p12 = new Point2D( (p1.x+p2.x)/2 , (p1.y+p2.y)/2 );
			var d12 = p2.subtract(p1).perp();
			d12 = d12.scale( disputeData[c].perpOffset / d12.length() );
			
			var circle = getCircle( disputantData[id1].position , disputantData[id2].position , p12.add( d12 ) );
			var t1 = THICKNESS*disputeData[c].count1/maxCount;
			var t2 = THICKNESS*disputeData[c].count2/maxCount;
			
			
			if( isInArc( p , circle , p1 , p12.add(d12) , t1 , (t1+t2)/2 ) || isInArc( p , circle , p12.add(d12) , p2 , (t1+t2)/2  , t2 ) )
			{
				selectedID = c;
				isDispute = true;
			}
		}
	}
	for (var c=0; c<disputantData.length; c++) {
		if (disputantData[c].position.x-setMaxWidth(c)/2<x && disputantData[c].position.x+setMaxWidth(c)/2>x && disputantData[c].position.y-HEIGHT/2<y && (disputantData[c].position.y+HEIGHT/2)>y)
		{
			selectedID = c;
			isDispute = false;
		}
	}
}
function mouseUpHandler() {
	selectedID = -1;
}
function mouseMoveHandler() {
	if (selectedID != -1) {
		if( isDispute )
		{
			// Disputant IDs
			var id1 = disputeData[selectedID].id1;
			var id2 = disputeData[selectedID].id2;

			// Curve end-points
			var p1 = disputantData[id1].position;
			var p2 = disputantData[id2].position;
			var p = new Point2D( event.x , event.y );
			
			// Circle through the end-points and the click-point
			var circle = getCircle( p1 , p2 , p );
						
			// Segment mid-point
			var p12 = p1.add(p2).scale(0.5);
			
			// Normalized segment perpendicular direction
			var d12 = p2.subtract(p1).perp();
			d12 = d12.scale( 1./d12.length() );

			var _p = p12.subtract( circle.c )
			_p = _p.scale( circle.r/_p.length() );
			
			if( p.subtract( p1 ).dot( d12 ) * _p.dot( d12 ) <0 ) _p = _p.scale(-1);
			_p = circle.c.add( _p );
	
			disputeData[selectedID].perpOffset =  ( _p.subtract(p12) ).dot( d12 );
		}
		else
		{
			if (event.x > YEAR_WIDTH) {
				disputantData[selectedID].position.x += event.x - mouseX ;
				mouseX = event.x;
			}
			if (!disputantData[selectedID].hasYear()) {
				disputantData[selectedID].position.y += event.y - mouseY ;
				mouseY = event.y;
			}
		}
		draw();
	}
}


function start()
{
//	document.body.style.overflow = "hidden";
	canvas = document.getElementById("mycanvas");
	canvas.width = window.innerWidth;
	canvas.height = window.innerHeight - document.getElementById('interface').offsetHeight;
	minYear = Infinity;
	maxYear = -Infinity;
	maxTotalDisputes = 0;
	for (var i=0; i < disputantData.length; i++) {
		if (disputantData[i].year < minYear) {
			minYear=disputantData[i].year;
		}
		if (disputantData[i].year > maxYear) {
			if (disputantData[i].year != Infinity)
			{
				maxYear=disputantData[i].year;
			}
		}
		if (disputantData[i].totalDisputes > maxTotalDisputes) {
			maxTotalDisputes = disputantData[i].totalDisputes;
		}
	}
	var diff = ( maxYear - minYear ) / 20;
	minYear -= diff;
	maxYear += diff;
	

	for( var i=0 ; i<disputantData.length ; i++ )
	{
		disputantData[i].position.x = Math.random() * (canvas.width - YEAR_WIDTH) + YEAR_WIDTH;
		if (disputantData[i].hasYear()) {
			disputantData[i].position.y = fractionToY( yearToFraction(disputantData[i].year) );
		}
		else {
			disputantData[i].position.y = fractionToY( 0.5 );
		}
	}
	draw();
	
	//for( var i=0 ; i<disputeData.length ; i++ ) console.log( disputeData[i].toString() ); 

	canvas.onmousedown = mouseDownHandler;
	canvas.onmouseup   =   mouseUpHandler;
	canvas.onmousemove = mouseMoveHandler;
//	canvas.addEventListener( 'mousemove' , mouseMoveHandler );
}
