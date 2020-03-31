var seija = require("seija");

exports.appVersion = seija.version;

exports._newSimple2d = function(cfg) {

  console.error(cfg);
  return seija.g2d.newSimple2d(cfg);
}

exports._newApp = function(s2d) {
  return function(cfg) {
    console.error(Object.keys(cfg));
    return seija.app.newApp(s2d,cfg);
  }
}

exports.runApp = function(app) {
  return function() {
    seija.app.runApp(app);
  }
}

function Fuck() {
  console.error("run on Fuck");
}