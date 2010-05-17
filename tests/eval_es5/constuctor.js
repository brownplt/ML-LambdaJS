constr = function() {
    this.x = 5;
};

a = new constr();

assertobj((a.x === 5 && a instanceof constr && a instanceof Object), "constructor");
