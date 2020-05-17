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
        return function(loaderConfig) {
          return function() {
            return seija.g2d.loadSync(loader,world,typid,path,loaderConfig.config);
          }
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
        var b = seija.g2d.addTransform(world,eid,prop.pos,prop.scale,prop.rotate);
        return b;
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
        if(prop.anchor) {
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
          var b = seija.g2d.addImageRender(world,eid,texId,prop.color);
          return b;
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
          var newEvent = defaultEvent();
          seija.g2d.attachNodeEvent(world,eid,evType,isCapture,newEvent);
          return newEvent;
        }
      }
    }
  }
}

exports._fetchTimeEvent = function(world) {
  return function(eid) {
    return function(prop) {
      return function() {
        var newEvent = defaultEvent();
        seija.g2d.attachTimeEvent(world,eid,prop[0],prop[1],newEvent);
        return newEvent;
      }
    }
  }
}

exports._fetchGlobalEvent = function (world) {
  return function(eid) {
    return function(typ) {
      return function() {
        var newEvent = defaultEvent();
        newEvent.func = function(val) {
          return {value0:val[0],value1:val[1]};
        };
        seija.g2d.attachGlobalEvent(world,eid,typ,newEvent);
        return newEvent;
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
        var val = f(a);
        return val;
      });
  }
}

exports._getViewPortSize = function(world) {
  return function() {
      return seija.g2d.getViewPortSize(world);
  }
}


exports._newBehavior = function(val) {
  return newBehavior(val);
}

exports._attachBehavior = function(ev) {
  return function(b) {
    return function() {
      ev.behavoirs.push(b);
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
  return b.value;
}


exports._setBehaviorCallback = function(b) {
  return function(f) {
    return function() {
      b.callBack = function(val) {
        f(val)();
      };
    }
  }
}

exports._setTransformBehavior = function(world) {
  return function(e) {
    return function(b) {
      return function() {
        setTransformBehavior(world,e,b);
      }
    }
  }
}


exports._setRect2dBehavior = function(world) {
  return function(e) {
    return function(b) {
      return function() {
        setRect2dBehavior(world,e,b);
      }
    }
  }
}

exports._setSpriteRenderBehavior = function(world) {
  return function(e) {
    return function(b) {
      return function() {
        setSpriteRenderBehavior(world,e,b);
      }
    }
  }
}


exports._setTextRenderBehavior = function(world) {
  return function(e) {
    return function(p) {
      return function() {
        setTextRenderBehavior(world,e,p);
      }
    }
  }
}

exports._addTransparent = function(world) {
  return function(e) {
    return function() {
      var b = seija.g2d.addTransparent(world,e);
      return b;
    }
  }
}

exports._addSpriteRenderByProp = function(world) {
  return function(e) {
    return function(sheet) {
      return function(spriteName) {
        return function(prop) {
          return function() {
            var b = seija.g2d.addSpriteRender(world,e,sheet,spriteName,prop.type,prop.color);
            return b;
          }
        }
      }
    }
  }
}

exports._addTextRenderByProp = function(world) {
  return function(e) {
    return function(fontId) {
      return function(prop) {
        return function() {
          return seija.g2d.addTextRender(world,e,fontId,prop.text,prop.color,prop.fontSize,prop.lineMode);
        }
      }
    }
  }
}

exports._getSpriteRectInfo = function(world) {
  return function(sheet) {
    return function(spriteName) {
      return seija.g2d.getSpriteRectInfo(world,sheet,spriteName);
    }
  }
}

exports._mergeEvent = function(eventArray) {
  return function() {
    return mergeEvent(eventArray);
  }
}

exports._mapBehavior = function(behavior) {
  return function(fn) {
    return fn(behavior.value);
  }
}

//do this
exports._tagBehavior = function(behavior) {
  return function(event) {
    return function() {
      return tagBehavior(behavior,event);
    }
  }
}

exports._setNextEvent = function(event) {
  return function(nextEvent) {
    return function() {
      return setNextEvent(event,nextEvent);
    }
  }
}

exports._newEvent = function() {
  return defaultEvent();
}

exports._listElement = function (world) {
  this.cacheList = [];
  var ref_this  = this;
  return function (parent) {
    return function(blist) {
      return function(f) {
        return function() {
            blist.callBack = function(newLst) {
              var dropList = [];
              var newList = [];
              var maxLen = Math.max(ref_this.cacheList.length,newLst.length);
              for(var i = 0; i < maxLen;i++) {
                var curOld = null;
                var curNew = null;
                if(newLst.length > i) {
                  curNew = newLst[i];
                }
                
                if(ref_this.cacheList.length > i) {
                  curOld = ref_this.cacheList[i];
                }
                if(curOld == null && curNew != null) {
                  newList.push(curNew);
                } else if(curNew == null && curOld != null) {
                  dropList.push(curOld);
                } else if (curNew.key != curOld.key) {
                  newList.push(curNew);
                  dropList.push(curOld);
                }
              }
              for(var j = 0; j < newList.length;j++) {
                  var eid = f(newLst[j])();
                  ref_this.cacheList.push({
                    data:newList[j],
                    eid: eid,
                    key: newList[j].key
                  });
              }
              for(var j = 0; j < dropList.length;j++) {
                seija.g2d.destoryEntity(world,dropList[j].eid);
              }
            };
        }
      }
    }
  }
}

exports.getChildrens = function(world) {
  return function(pe) {
    return function() {
      var arr = seija.g2d.getChildrens(world,pe);
      return arr;
    }
  }
}

exports.removeAllChildren = function(world) {
  return function(pe) {
    return function() {
      seija.g2d.removeAllChildren(world,pe);
    }
  }
}

exports.unsafeShow = function(val) {
  return function() {
    console.error(val);
  }
}

/*============================================*/
function defaultEvent() {
  var retObject = {
      nextEvents:[],
      behavoirs:[],
      tagBehaviors:[],
      func:null,
      onFire: function(val) {
          var newVal = val;
          if(this.func != null) {
              newVal = this.func(val);
          }

          for(var i = 0;i < this.nextEvents.length;i++) {
              var curEvent = this.nextEvents[i];
              curEvent.onFire(newVal);
          }

          for(var i = 0;i < this.behavoirs.length;i++) {
              var curBehavior = this.behavoirs[i];
              curBehavior.onValue(newVal);
          }

          for(var i = 0;i < this.tagBehaviors.length;i++) {
            var [b,e] = this.tagBehaviors[i];
            e.onFire(b.value);
          }
      }
  };
  retObject.onFire.bind(retObject);
  return retObject;
}

function chainEvent(ev,fn) {
  var newEvent = defaultEvent();
  newEvent.func = fn;
  ev.nextEvents.push(newEvent);
  return newEvent;
}

function mergeEvent(eventArray) {
  var newEvent = defaultEvent();
  for(var i = 0; i < eventArray.length;i++) {
    var curEv = eventArray[i];
    curEv.nextEvents.push(newEvent);
  }
  return newEvent;
}

function setNextEvent(event,nextEvent) {
  event.nextEvents.push(nextEvent);
}

function tagBehavior(b,ev) {
  var newEvent = defaultEvent();
  ev.tagBehaviors.push([b,newEvent]);
  return newEvent;
}


function newBehavior(val) {
  var retObject = {
      value:val,
      foldFunc:null,
      callBack:null,
      attachInfo:null,
      attchCallback:null,
      onValue: function(eVal) {
          if(this.foldFunc != null) {
              this.value = this.foldFunc(this.value,eVal);
          } else {
              this.value = eVal;
          }
          if(this.callBack != null) {
              this.callBack(this.value);
          }
          if(this.attchCallback != null) {
              this.attchCallback(this.value);
          }
      }
  };
  retObject.onValue.bind(retObject);
  return retObject;
}

function setBehaviorFoldFunc(b,f) {
  b.foldFunc = f;
}

function setTransformBehavior(world,entity,prop) {
  if(prop.pos) {
      prop.pos.attchCallback = function(val) {
          seija.g2d.setTransform(world,entity,0,val);
      }.bind(prop.pos);
  }
  if(prop.scale) {
      prop.scale.attchCallback = function(val) {
        seija.g2d.setTransform(world,entity,1,val);
      }.bind(prop.scale);
  }
  if(prop.rotation) {
      prop.rotation.attchCallback = function(val) {
        seija.g2d.setTransform(world,entity,2,val);
      }.bind(prop.rotation);
  }
}

function setRect2dBehavior(world,entity,prop) {
  if(prop.size) {
    prop.size.attchCallback = function(val) {
      seija.g2d.setRect2d(world,entity,0,val);
    }.bind(prop.size);
  }
  if(prop.anchor) {
    prop.anchor.attchCallback = function(val) {
      seija.g2d.setRect2d(world,entity,1,val);
    }.bind(prop.anchor);
  }
}

function setSpriteRenderBehavior(world,entity,prop) {
  if(prop.spriteName) {
    prop.spriteName.attchCallback = function(val) {
      seija.g2d.setSpriteRender(world,entity,1,val);
    }.bind(prop.spriteName);
  }
  if(prop.color) {
    prop.color.attchCallback = function(val) {
      seija.g2d.setSpriteRender(world,entity,0,val);
    }.bind(prop.color);
  }
}

function setTextRenderBehavior(world,entity,prop) {
  if(prop.text) {
    prop.text.attchCallback = function(val) {
      seija.g2d.setSpriteRender(world,entity,0,val);
    }.bind(prop.text);
  }
  if(prop.fontSize) {
    prop.fontSize.attchCallback = function(val) {
      seija.g2d.setSpriteRender(world,entity,1,val);
    }.bind(prop.fontSize);
  }
  if(prop.color) {
    prop.color.attchCallback = function(val) {
      seija.g2d.setSpriteRender(world,entity,2,val);
    }.bind(prop.color);
  }
  if(prop.anchor) {
    prop.anchor.attchCallback = function(val) {
      seija.g2d.setSpriteRender(world,entity,3,val);
    }.bind(prop.anchor);
  }
  if(prop.lineMode) {
    prop.lineMode.attchCallback = function(val) {
      seija.g2d.setSpriteRender(world,entity,4,val);
    }.bind(prop.lineMode);
  }
}

