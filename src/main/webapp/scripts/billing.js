$(document).ready(function(){
	$("#pin").hide();	
});

$(document).ready(function(){
	$("select#type").change(function(){
		var selection = $("select#type option:selected").val();
		
		if(selection == "mastercard" || selection == "visa"){
			$("#securityCode").show();
			$("#issuer").show();
			$("#pin").hide();
		} else if(selection == "ec"){
			$("#securityCode").hide();
			$("#issuer").hide();
			$("#pin").show();
		}
	});
});