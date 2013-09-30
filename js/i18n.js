

// Internationalization
// Search KEY into an array and return the associated value
function getI18nText(key) {

    try {
        var str = i18n[key];
        if (str === undefined) {
            str = key;
        }
        return str;

    } catch (e) {
        return key;
    }
}

    
    
