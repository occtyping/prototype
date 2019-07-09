window.addEventListener ("load", () => {
    let code = document.getElementById ("code");
    let select = document.getElementById ("select");
    let lock = false;
    let output = document.getElementById("output");

    function getFile(url) {
        let xhr = new XMLHttpRequest();
        xhr.open("GET", url);
        xhr.overrideMimeType("text/plain");
        xhr.addEventListener("readystatechange", () => {
            if (xhr.readyState == 4 && xhr.status == 200) {
                code.value = xhr.responseText;
                output.innerHTML = "";
                lock = false;
            }
        });
        xhr.send();
    };


    select.addEventListener ("change", () => {
        let n = +(select.value);
        if (n >= 1 && n <= 9 && !lock) {
            lock = true;
            getFile(n + ".ml");
        }

    });

});
