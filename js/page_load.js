function on_page_load(header_text) {
    // Set placeholder text for table filters
    $(".rt-filter").attr("placeholder", "search...")
    // Hide loading div and show rest of report
    $('#loading').hide();
    $('.section.level2')[1].style.display = "block"
    $('#footer')[0].style.display = "block";
    // Define icons for section dropdowns
    var icons = [
        "./icons/icons8-doughnut-chart-64.png",
        "./icons/icons8-map-64.png",
        "./icons/icons8-vertical-timeline-64.png",
        "./icons/icons8-data-sheet-64.png",
        "./icons/icons8-grid-64.png"
    ]
    // Insert icons in dropdowns ! will break if Analysis header changes...
    for (var i = 1; i <= icons.length; i++) {
        var sel = header_text + " > ul > li:nth-child(" + i + ") > a";
        var elem = document.querySelector(sel);
        var img = document.createElement("img");
        img.setAttribute("src", icons[i - 1]);
        img.style.width = "40px";
        img.style.marginRight = "5px";
        elem.prepend(img);
    }
};