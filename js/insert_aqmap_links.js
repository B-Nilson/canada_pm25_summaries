function insert_aqmap_links() {
    const elements = document.querySelectorAll("[data-lng], [data-lat]");
    elements.forEach(element => {
        const lng = element.getAttribute("data-lng");
        const lat = element.getAttribute("data-lat");
        const zoom = element.getAttribute("data-zoom") ?? 12;
        element.innerHTML = make_aqmap_link(element.innerText, lng, lat, zoom);
    });
}

function make_aqmap_link(label, lng, lat, zoom = 12) {
    let label_safe = label.replace(/'/g, "&rsquo;").replace(/"/g, "&quot;");
    return `<a title="${label_safe}" href="https://aqmap.ca/aqmap/en/#${zoom}/${lat}/${lng}">${label}</a>`
}
