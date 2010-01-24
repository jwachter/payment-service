$(document).ready(function(){
	$("#pinContainer").hide();	
});

$(document).ready(function(){
	$("select#type").change(function(){
		var selection = $("select#type option:selected").val();
		
		if(selection == "mastercard" || selection == "visa"){
			$("#securityCodeContainer").show();
			$("#issuerContainer").show();
			$("#pinContainer").hide();
		} else if(selection == "ec"){
			$("#securityCodeContainer").hide();
			$("#issuerContainer").hide();
			$("#pinContainer").show();
		}
	});
});