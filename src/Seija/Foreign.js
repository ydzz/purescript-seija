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

exports.setParent = function(world) {
  return function(eid) {
    return function(pid) {
      return function() {
        return seija.g2d.setParent(world,eid,pid);
      }
    }
  }
}

exports.getEvent = function(world) {
  return function(eid) {
    return function(evType) {
      return function(isCapture) {
        return function() {
          return seija.g2d.getEvent(world,eid,evType,isCapture);
        }
      }
    }
  }
}

exports.chainEventEffect = function(ev) {
  return function(fn) {
    return function() {
      chainEvent(ev,function(val) {
        fn(val)();
        return null;
      });
    }
  }
}
exports.chainEvent = function(ev) {
  return function(f) {
      return chainEvent(ev,function(a) {
        var str = f(a);
        return str;
      });
  }
}
exports._getViewPortSize = function(world) {
  return function() {
      return seija.g2d.getViewPortSize(world);
  }
}

exports._newBehavior = function(val) {
  console.error(val);
  return seija.g2d.newBehavior(val);
}

exports._attachBehavior = function(ev) {
  return function(b) {
    return function() {
      return seija.g2d.attachBehavior(ev,b);
    }
  }
}

exports._setBehaviorFoldFunc = function(b) {
  return function(f) {
    return function() {
      setBehaviorFoldFunc(b,function(val,ev) {
        var retVal = f(val)(ev)
        return retVal;
      });
    }
  }
}

exports._getBehaviorValue = function(b) {
  return seija.g2d.getBehaviorValue(b);
}

exports._setBehaviorCallback = function(b) {
  return function(f) {
    return function() {
      b.callFunc = function(val) {
        f(val)();
      }
      seija.g2d.setBehaviorCallback(b,b.callFunc);
    }
  }
}

exports._setRect2dBehavior = function(world) {
  return function(e) {
    return function(b) {
      return function() {
        seija.g2d.setRect2dBehavior(world,e,b);
      }
    }
  }
}

exports._setTransformBehavior = function(world) {
  return function(e) {
    return function(b) {
      return function() {
        seija.g2d.setTransformBehavior(world,e,b);
      }
    }
  }
}

function chainEvent(event,f) {
  var newEvent = seija.g2d.chainEvent(event,f);
  newEvent.f = f;
  if(event.childrens == undefined) {
      event.childrens = [];
  }
  event.childrens.push(newEvent);
  return newEvent;
}

function setBehaviorFoldFunc(b,f) {
  b.myfunc = f;
  seija.g2d.behaviorSetFoldFunc(b,b.myfunc);
}