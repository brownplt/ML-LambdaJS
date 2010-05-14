constr = function() {
    this.x = 5;
};

a = new constr();

assert((a.x === 5 && a instanceof constr && a instanceof Object), "constructor");
