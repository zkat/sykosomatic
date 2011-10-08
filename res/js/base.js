var sykosomatic =
(function () {

    var initializers = [];
    function init (){
        for (var i in initializers) {
            initializers[i]();
        }
    }
    function register_initializer(initializer){
        initializers.push(initializer);
    }
    pub = {init:init,register_initializer:register_initializer};
    return pub;
})();
$(document).ready(sykosomatic.init);