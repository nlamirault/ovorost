//<![CDATA[

//if (GBrowserIsCompatible()) {

var sidebar_html = "";
var gmarkers = null;
var htmls = null;
var map = null;

var user_lat = null;
var user_lng = null;
var user_sex = null;
var user_gravatar = "";


function selectCountryStats() {
    var country = document.aboutForm.countries.options[document.aboutForm.countries.selectedIndex].value;
    document.aboutForm.action = "/about?country=" + country;
    document.aboutForm.submit();
}


function selectCountry() {
    var country = document.map.countries.options[document.map.countries.selectedIndex].value;
    document.map.action = "/index?country=" + country;
    document.map.submit();
}




function selectResort() {
    var resort = document.map.resorts.options[document.map.resorts.selectedIndex].value;
    if (resort != "null") {
        var index = resort.indexOf(",");
        if (index != -1) {
/*             show(resort.substr(0, index), */
/*                  resort.substr(index + 1, resort.length)); */
            var i = resort.substr(0, index);
            var j = resort.substr(index + 1, resort.length);
            GEvent.trigger(gmarkers[i][j], "click");
        }
    }
}


function selectEvent() {
    var index  = document.map.events.options[document.map.events.selectedIndex].value;
    var events = gmarkers[0];
    GEvent.trigger(events[index], "click");
}


// Reload the side bar
function reloadSideBar() {

    //sidebar_html = "Stations<br><br>";

    sidebar_html = "<select name=\"resorts\" onChange=\"selectResort()\">";
    sidebar_html += "<option value=\"null\">" + getI18nText("Resorts") +  "</option>";

    if (htmls != null) {
        for (var i = 0; i < htmls.length; i++) {
            if (htmls[i] != null) {
                for (var j = 0; j < htmls[i].length; j++) {
                    var label = htmls[i][j][0];
                    sidebar_html += "<option value=\"" + i +","
                        + j + "\"> " + label + "</option>";
                }
            }
        }
    }
    sidebar_html += "</select>";
    document.getElementById("resortsbar").innerHTML = sidebar_html;
}


// Update the selection of events
/* function reloadEvents() { */

/*     var events = "<select name=\"events\" onChange=\"selectEvent()\">"; */
/*     events += "<option value=\"null\"> Events </option>"; */
/*     events += "</select>"; */
/*     document.getElementById("eventsSelection").innerHTML = events; */
/* } */



// Get differents icons from index
function get_icon(index) {

    var icon = new GIcon();
    var color;
    var nb = (index % 12);
    switch (nb) {
    case 1: color = "red";
        break;
    case 2: color = "blue";
        break;
    case 3: color = "purple";
        break;
    case 4: color = "orange";
        break;
    case 5: color = "yellow";
        break;
    case 6: color = "green";
        break;
    case 7: color = "brown";
        break;
    case 8: color = "white";
        break;
    case 9: color = "brown";
        break;
    case 10: color = "gray";
        break;
    case 11: color = "black";
        break;
    case 12: color = "yellow";
        break;
    }
    icon.image = "http://labs.google.com/ridefinder/images/mm_20_" + color + ".png";
    icon.shadow = "http://labs.google.com/ridefinder/images/mm_20_shadow.png";
    icon.iconSize = new GSize(12, 20);
    icon.shadowSize = new GSize(22, 20);
    icon.iconAnchor = new GPoint(6, 20);
    icon.infoWindowAnchor = new GPoint(5, 1);
    return icon;
}


// Get a new icon with man repre
function get_man_icon() {

    //alert("Get man icon");
    var icon = new GIcon();
    icon.image = "http://maps.google.com/mapfiles/ms/micons/man.png";
    icon.iconSize = new GSize(32, 32);
    icon.shadowSize = new GSize(32, 32);
    icon.iconAnchor = new GPoint(6, 20);
    icon.infoWindowAnchor = new GPoint(5, 1);
    return icon;
}


// Get a new icon with woman representation.
function get_woman_icon() {

    //alert("Get woman icon");
    var icon = new GIcon();
    icon.image = "http://maps.google.com/mapfiles/ms/micons/woman.png";
    icon.iconSize = new GSize(32, 32);
    icon.shadowSize = new GSize(32, 32);
    icon.iconAnchor = new GPoint(6, 20);
    icon.infoWindowAnchor = new GPoint(5, 1);
    return icon;
}


// Get a new person icon.
function get_person_icon (sex) {

    //alert("Icon for " + sex + ("F" == sex));
    var icon = null;
    if ("M" == sex)  {
        icon = get_man_icon();

    } else if ("F" == sex) {
        icon = get_woman_icon();

    } else  {
        //alert("No icon for this user " + sex);
    }
    return icon;    
}


// Get a new icon with snow representation.
function get_event_icon() {

    var icon = new GIcon();
    icon.image = "http://maps.google.com/mapfiles/ms/micons/snowflake_simple.png";
    icon.iconSize = new GSize(32, 32);
    icon.shadowSize = new GSize(34, 20);
    icon.iconAnchor = new GPoint(6, 20);
    icon.infoWindowAnchor = new GPoint(5, 1);
    return icon;
}



// A function which creates a tab pane.
// DATA is an array which contains informations about XML entry.
function make_infos_tab(data) {

    var slopes = data[4].split("-");
    var text = "";

    if (slopes.length == 4) {
        text = '<div page="1" label="General" class="active">'
            + '<div id"=slope_info"> ' + getI18nText("Slopes") + ' : '
            + '<span id="slope_green">' + slopes[0] + '</span>'
            + '<span id="slope_blue">' + slopes[1] + '</span>'
            + '<span id="slope_red">' + slopes[2] + '</span>'
            + '<span id="slope_black">' + slopes[3] + '</span>'
            + '</div>'
            + '<div id="slope_info"> ' + getI18nText("Snow") + ': ' + data[5] + '</div>'
            + '<div id="slope_info"> ' + getI18nText("Avalanche")
            + ' <img src="' + data[6] + '" style="vertical-align:middle" ></div>'
            + '<div id="slope_info"> '+ getI18nText("Updated") + ' : ' + data[7] + '</div>'
            + '</div>';

    } else  {
        text = '<div page="1" label="General" class="active">'
            + '<div id="slope_info"> ' + getI18nText("Slopes") + ' : ' + data[4] + '</div>'
            + '<div id="slope_info"> ' + getI18nText("Snow") + ' : ' + data[5] + '</div>'
            + '<div id="slope_info"> ' + getI18nText("Avalanche") + ' <img src="' + data[6] + '"></div>'
            + '<div id="slope_info"> ' + getI18nText("Updated") + ' : ' + data[7] + '</div>'
            + '</div>';
    }
    return text;
}


// Creates informations tab window.
// DATA is an array which contains informations about XML entry.
function make_tab_general(data) {

    var text = '<div><b>' + data[0] + '</b></div>'
        + '<div> ' + getI18nText("Website")
        + ': </div><div><a href="' + data[9] + '">'
        + data[9] + '</a></div>';
    return text;
}


// Creates a new marker on map by using POINT, and add a listener
// which display HTML text
function createTabbedMarker(point, data, index) {

    var marker = new GMarker(point, get_icon(index));
    var infos = make_infos_tab(data);
    var general = make_tab_general(data);
    var infoTabs = [
                    new GInfoWindowTab(getI18nText("General"), general),
                    new GInfoWindowTab(getI18nText("Info"), infos)
                    ];
    GEvent.addListener(marker, "click", function() {
            marker.openInfoWindowTabsHtml(infoTabs);
        });
    GEvent.addListener(marker, "dragstart", function() {
            map.closeInfoWindow();
        });
    GEvent.addListener(marker, "dragend", function() {
            marker.openInfoWindowHtml("Bien joué !");
        });
    return marker;
}



// Creates user informations tab window.
function make_user_tab_infos(lat, lng, uri) {

    var text = '<div><b> Latitude </b>: ' + lat + '</div>'
        + '<div><b> Longitude </b>: ' + lng + '</div>'
        + '<div><img src="' + uri + '"></div>';
        return text;
}



// Creates a new marker for an user.
function create_user_marker() {

    if ((user_lng != null) && (user_lat != null) && (user_sex != "")) {
        //alert("User lat=" + user_lat + " lng=" + user_lng);
        
        var userPoint = new GPoint(user_lng, user_lat);
        var userMarker = new GMarker(userPoint, get_person_icon(user_sex));
        var infos = make_user_tab_infos(user_lat, user_lng, user_gravatar);
        GEvent.addListener(userMarker, "click", function() {
                userMarker.openInfoWindowHtml(infos);
            });
        GEvent.addListener(userMarker, "dragstart", function() {
                map.closeInfoWindow();
            });
        GEvent.addListener(userMarker, "dragend", function() {
                marker.openInfoWindowHtml("Bien joué !");
            });
        map.addOverlay(userMarker);
    }
}


// Creates event informations tab window.
function make_event_tab_infos(title, label, from, to, url) {
    var text = '<div><b>' + title + '</b></div>'
        + '<div> ' + getI18nText("Place") + ' : ' + label + '</div>'
        + '<div> '+ getI18nText("From") + ' ' + from + ' '
        + getI18nText("To") + ' ' + to + '</div>'
        + '<div><a href=\"' + url + '\"> Description </a></div>';
    return text;
}



// Creates a new marker for an user.
function createEventMarker(eventPoint, title, label, from, to, url) {

/*     alert("Marker Event=" + title + " label=" + label */
/*           + " from=" + from + " to=" + to + " url= " + url); */
        
    var eventMarker = new GMarker(eventPoint, get_event_icon());
    var infos = make_event_tab_infos(title, label, from, to, url);
    GEvent.addListener(eventMarker, "click", function() {
            eventMarker.openInfoWindowHtml(infos);
        });
    GEvent.addListener(eventMarker, "dragstart", function() {
            map.closeInfoWindow();
        });
    GEvent.addListener(eventMarker, "dragend", function() {
            marker.openInfoWindowHtml("Bien joué !");
        });
    map.addOverlay(eventMarker);
    return eventMarker;
}


// Create a new Google Map
function createMap(latitude, longitude, level, ulat, ulng, sex, gravatar) {

/*      alert("Map " + latitude + ":" + longitude + "=" + level  */
/*            + " user=" + ulat +"," + ulng + "," + sex */
/*            + " gravatar=" + gravatar); */
    
    if (gmarkers == null) {
        gmarkers = [];
        htmls = [];
    }

    if ((ulat != 0) && (ulng != 0) && (sex != "")) {
        user_lat = ulat;
        user_lng = ulng;
        user_sex = sex;
        user_gravatar = gravatar;        
    }

    reloadSideBar();

    map = new GMap2(document.getElementById("map"));
    map.addControl(new GLargeMapControl());
    map.addControl(new GMapTypeControl());
    map.setCenter(new GLatLng(latitude, longitude), level);

    create_user_marker();

    return map;
}



function loadEvents() {

    var url = "/ajax-function/?ajax-fun=GET-USER-EVENTS&ajax-xml=true";
    //alert("GET " + url);

    var request = GXmlHttp.create();
    request.open("GET", url, true);
    request.onreadystatechange = function() {
        if (request.readyState == 4) {
            if (request.status != 200) {
                alert("file not found");
                return;
            }

            var xmlDoc = request.responseXML;
            //alert("Doc " + xmlDoc);
            if (!xmlDoc) {
                alert("invalid xml file");
                return;
            }

            var events  = xmlDoc.documentElement.getElementsByTagName("marker");
            //alert("Events " + events.length);
            
            var longitude;
            var latitude;
            var label;
            var title;
            var begin;
            var end;

            var domaines = [];
            var labels = [];
            var data = [];

            events_html = "<select name=\"events\" onChange=\"selectEvent()\">";
            events_html += "<option value=\"null\"> " + getI18nText("Events") +" </option>";

            for (var i = 0; i < events.length; i++) {
                title = events[i].getAttribute("title");
                begin = events[i].getAttribute("begin_date");
                end = events[i].getAttribute("end_date");
                longitude = parseFloat(events[i].getAttribute("lng"));
                latitude = parseFloat(events[i].getAttribute("lat"));
                label = events[i].getAttribute("label");
                urlDesc = events[i].getAttribute("url");                
                
/*                 alert("Event long=" + longitude + " lat=" + latitude + " " */
/*                       + " label=" + label + " title=" + title */
/*                       + " from=" + begin + " to=" + end */
/*                       + ' url=' + urlDesc); */

                var data = [];
                data[0] = label;
                data[1] = title;
                data[2] = begin;
                data[3] = end;
                labels[i] = data;

                var point = new GPoint(longitude, latitude);
                var marker = createEventMarker(point, title, label, begin, end, urlDesc);
                //map.addOverlay(marker);

                domaines[i] = marker;

                events_html += "<option value=\"" + i + "\"> " + title + "</option>";
            }

            gmarkers[0] = domaines;
            htmls[0] = labels;

            events_html += "</select>";
            document.getElementById("events_selection").innerHTML = events_html;
        }
    }
    request.send(null);
}


// Remove user's events markers from map
function unloadEvents() {

    var markers = gmarkers[0];
    for (var i = 0; i < markers.length; i++) {
        map.removeOverlay(markers[i]);
    }
    gmarkers[0] = null;
    htmls[0] = null;

    document.getElementById("events_selection").innerHTML = "";
}


// Display or hide user's events on map.
function getEvents() {

    var markers = gmarkers[0];
    if (markers != null){
        unloadEvents();        
        
    } else  {
        loadEvents();
    }
}


// Remove markers and labels corresponding to database identified
// by his index
function onUnloadFile(index) {

    var markers = gmarkers[index];
    for (var i = 0; i < markers.length; i++) {
        map.removeOverlay(markers[i]);
    }
    gmarkers[index] = null;
    htmls[index] = null;
    reloadSideBar();
}

// Retreive data from file, and update the map
function onLoadFile(index, country, region) {

    var url = "/ajax-function/?ajax-fun=GET-SKI-RESORTS&ajax-xml=true"
        + "&ajax-1=" + country + "&ajax-2=" + region;
    //alert("GET " + url);

    var request = GXmlHttp.create();
    request.open("GET", url, true);
    request.onreadystatechange = function() {
        if (request.readyState == 4) {
            if (request.status != 200) {
                alert("file not found");
                return;
            }

            var xmlDoc = request.responseXML;
            //alert("Doc " + xmlDoc);
            if (!xmlDoc) {
                alert("invalid xml file");
                return;
            }

            var stations = xmlDoc.documentElement.getElementsByTagName("marker");
            //alert("stations " + stations + "=" + stations.length);

            var longitude;
            var latitude;
            var html;
            var label;
            var min;
            var max;
            var slopes;
            var snow;
            var avalanche;
            var updated;
            var domaines = [];
            var labels = [];
            var data = [];

            for (var i = 0; i < stations.length; i++) {
                longitude = parseFloat(stations[i].getAttribute("lng"));
                latitude = parseFloat(stations[i].getAttribute("lat"));
                html = stations[i].getAttribute("html");
                label = stations[i].getAttribute("label");
                min = stations[i].getAttribute("min");
                max = stations[i].getAttribute("max");
                slopes = stations[i].getAttribute("slopes");
                snow = stations[i].getAttribute("snow");
                avalanche = stations[i].getAttribute("avalanche");                
                updated = stations[i].getAttribute("updated");
                web = stations[i].getAttribute("web");
                
/*                  alert("Station long=" + longitude + " lat=" + latitude + " " */
/*                        + html + " " + min + " " + max + " " + web */
/*                        + " slopes=" + slopes + " snow=" + snow */
/*                        + " avalanche=" + avalanche); */

                var data = [];
                data[0] = html;
                data[1] = label;
                data[2] = min;
                data[3] = max;
                data[4] = slopes;
                data[5] = snow;
                data[6] = avalanche;
                data[7] = updated;
                data[9] = web;
                labels[i] = data;

                var point = new GPoint(longitude, latitude);
                var marker = createTabbedMarker(point, data, index);
                map.addOverlay(marker);

                domaines[i] = marker;

            }
            gmarkers[index] = domaines;
            htmls[index] = labels;
            reloadSideBar();
        }
    }
    request.send(null);
}



// Add or remove resorts from the map.
function setResorts(index, country, region) {

    var markers = gmarkers[index];
    if (markers != null) {
        onUnloadFile(index, country, region);
        
    } else {
        onLoadFile(index, country, region);
    }
}


//} else {
//  alert("Sorry, the Google Maps API is not compatible with this browser");
9//}

//]]>
