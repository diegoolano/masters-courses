function toggle_login(){

    var logged_in_user_id_for_demo = 8170;
	c = getCookie("userid");
	if( c == "" || c == 0){
		console.log("no cookie for userid found or cookie 0 found so set it");
		delete_cookie('roundcube_sessauth');
		setCookie("userid",logged_in_user_id_for_demo,20);
	}
	else
	{
	    delete_cookie('userid');
		console.log("found cookie: "+c+", so set it to 0");
		setCookie("userid",0,20);
	}
	window.location.reload(true); 
}

function switch_to_binary()
{
    setCookie("ratingstype","binary",20);
    window.location.reload(true);
}

function switch_to_decimal()
{
    setCookie("ratingstype","decimal",20);
    window.location.reload(true);
}

var delete_cookie = function(name) {
    document.cookie = name + '=;expires=Thu, 01 Jan 1970 00:00:01 GMT;';
};

function setCookie(cname, cvalue, exdays) {
    var d = new Date();
    d.setTime(d.getTime() + (exdays*24*60*60*1000));
    var expires = "expires="+d.toGMTString();
    document.cookie = cname + "=" + cvalue + ";path=/; " + expires;
}

function getCookie(cname) {
    var name = cname + "=";
    var ca = document.cookie.split(';');
    for(var i=0; i<ca.length; i++) {
        var c = ca[i].trim();
        if (c.indexOf(name) == 0) return c.substring(name.length,c.length);
    }
    return "";
}
