var seija = require("seija");

exports.appVersion = seija.version;

exports._newSimple2d = function(cfg) {
  return seija.g2d.newSimple2d(cfg);
}

exports._newApp = function(s2d) {
  return function(cfg) {
    if(cfg.OnStart) {
      var onStartFn = cfg.OnStart;
      cfg.OnStart = function(world) {
        onStartFn(world)();
      }
    };
    if(cfg.OnUpdate) {
      var onUpdateFn = cfg.OnUpdate;
      cfg.OnUpdate = function(world) {
        onUpdateFn(world)();
      }
    };
    if(cfg.OnQuit) {
      var onQuitFn = cfg.OnQuit;
      cfg.OnQuit = function(world) {
        onQuitFn(world)();
      }
    };
    return seija.app.newApp(s2d,cfg);
  }
}

exports.runApp = function(app) {
  return function() {
    seija.app.runApp(app);
  }
}

exports.fetchLoader = function(world) {
  return function() {
    return seija.g2d.fetchLoader(world);
  }
}

exports.loadAssetSync = function(world) {
  return function(loader) {
    return function(typid) {
      return function(path) {
        return function() {
          return seija.g2d.loadSync(loader,world,typid,path);
        }
      }
    }
  }
}

exports.newEntity = function(world) {
  return function () {
    return seija.g2d.newEntity(world);
  }
}

exports.addCABEventRoot = function (world) {
  return function (eid) {
    return function () {
      return seija.g2d.addCABEventRoot(world,eid);
    }
  }
}

exports._getTextureSize = function(world) {
  return function(texId) {
    return seija.g2d.getTextureSize(world,texId);
  }
}

exports.addTransformByProp = function (world) {
  return function(eid) {
    return function(prop) {
      return function() {
        return seija.g2d.addTransform(world,eid,prop.pos,prop.scale,prop.rotate);
      }
    }
  }
}

exports.addRect2DByProp = function (world) {
  return function(eid) {
    return function(prop) {
      return function() {
        var sizeW = null;
        var sizeH = null;
        var anchorX = 0.5;
        var anchorY = 0.5;
        if (prop.size) {
          sizeW = prop.size[0];
          sizeH = prop.size[1];
        }
        if(prop.anchorX) {
          anchorX = prop.anchor[0];
          anchorY = prop.anchor[1];
        }
        return seija.g2d.addRect2d(world,eid,sizeW,sizeH,anchorX,anchorY);
      }
    }
  }
}

exports.addImageRenderByProp = function(world) {
  return function(eid) {
    return function(texId) {
      return function(prop) {
        return function() {
          return seija.g2d.addImageRender(world,eid,texId,prop.color);
        }
      }
    }
  }
}