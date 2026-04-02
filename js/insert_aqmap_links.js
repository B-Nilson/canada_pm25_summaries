function insert_aqmap_links() {
    const elements = document.querySelectorAll("[data-lng], [data-lat]");
    elements.forEach(element => {
        const lng = element.getAttribute("data-lng");
        const lat = element.getAttribute("data-lat");
        const zoom = element.getAttribute("data-zoom") ?? 12;
        const link = make_aqmap_link(element.innerText, lng, lat, zoom);
        const new_elem_parent = new DOMParser().parseFromString(link, "text/xml")
        element.replaceWith(new_elem_parent.firstChild);
    });
}

function make_aqmap_link(label, lng, lat, zoom = 12) {
    let label_safe = label.replace(/'/g, "&apos;").replace(/"/g, "&quot;");
    return `<a title="${label_safe}" href="https://aqmap.ca/aqmap/en/#${zoom}/${lat}/${lng}">${label}</a>`
}
