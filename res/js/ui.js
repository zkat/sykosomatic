sykosomatic.ui = (function () {

    function init() {
        $('.button,button').button();
        $('select').chosen();
    }

    return {init:init};

})();
sykosomatic.register_initializer(sykosomatic.ui.init);