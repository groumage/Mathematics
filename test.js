// Generated by js_of_ocaml 3.1.0
(function(c){"use strict";var
ap=" : flags Open_text and Open_binary are not compatible",ao=224,ay="Sys_error",aH="Invalid_argument",aG='"',t=1024,N="jsError",an="input",ai=785140586,ax=57343,aF=512,aw=982028505,aE="End_of_file",aD="failed to find '",aC="Failure",ah="Undefined_recursive_module",av=" : flags Open_rdonly and Open_wronly are not compatible",au="([^/]*)",am="Stack_overflow",b="",g=128,M="^",af="test.ml",ag=56320,ar=" : file already exists",as=240,at=2048,f=248,al="Not_found",ae="Assert_failure",n="/",aA="Sys_blocked_io",aB="fd ",aq="Out_of_memory",ak="Match_failure",ad="textarea",aj="static/",az="Division_by_zero";function
S(d,e,c){var
b=new
Array(c);for(var
a=0;a<c;a++)b[a]=d[e+a];return b}function
R(c,d,a){var
e=String.fromCharCode;if(d==0&&a<=4096&&a==c.length)return e.apply(null,c);var
f=b;for(;0<a;d+=t,a-=t)f+=e.apply(null,S(c,d,Math.min(a,t)));return f}function
P(b){if(c.Uint8Array)var
d=new(c.Uint8Array)(b.l);else
var
d=new
Array(b.l);var
f=b.c,e=f.length,a=0;for(;a<e;a++)d[a]=f.charCodeAt(a);for(e=b.l;a<e;a++)d[a]=0;b.c=d;b.t=4;return d}function
q(d,e,b,f,c){if(c==0)return 0;if(f==0&&(c>=b.l||b.t==2&&c>=b.c.length)){b.c=d.t==4?R(d.c,e,c):e==0&&d.c.length==c?d.c:d.c.substr(e,c);b.t=b.c.length==b.l?0:2}else
if(b.t==2&&f==b.c.length){b.c+=d.t==4?R(d.c,e,c):e==0&&d.c.length==c?d.c:d.c.substr(e,c);b.t=b.c.length==b.l?0:2}else{if(b.t!=4)P(b);var
g=d.c,h=b.c;if(d.t==4)if(f<=e)for(var
a=0;a<c;a++)h[f+a]=g[e+a];else
for(var
a=c-1;a>=0;a--)h[f+a]=g[e+a];else{var
i=Math.min(c,g.length-e);for(var
a=0;a<i;a++)h[f+a]=g.charCodeAt(e+a);for(;a<c;a++)h[f+a]=0}}return 0}function
bA(c,e){var
d=c.length,b=new
Array(d+1),a=0;for(;a<d;a++)b[a]=c[a];b[a]=e;return b}function
C(b,a){if(b.fun)return C(b.fun,a);var
c=b.length,d=a.length,e=c-d;if(e==0)return b.apply(null,a);else
if(e<0)return C(b.apply(null,S(a,0,c)),S(a,c,d-c));else
return function(c){return C(b,bA(a,c))}}function
bo(c,a){if(a.repeat)return a.repeat(c);var
d=b,e=0;if(c==0)return d;for(;;){if(c&1)d+=a;c>>=1;if(c==0)return d;a+=a;e++;if(e==9)a.slice(0,1)}}function
aK(a){if(a.t==2)a.c+=bo(a.l-a.c.length,"\0");else
a.c=R(a.c,0,a.c.length);a.t=0}function
aL(a){if(a.length<24){for(var
b=0;b<a.length;b++)if(a.charCodeAt(b)>127)return false;return true}else
return!/[^\x00-\x7f]/.test(a)}function
bv(f){for(var
l=b,d=b,i,h,j,a,c=0,k=f.length;c<k;c++){h=f.charCodeAt(c);if(h<g){for(var
e=c+1;e<k&&(h=f.charCodeAt(e))<g;e++);if(e-c>aF){d.substr(0,1);l+=d;d=b;l+=f.slice(c,e)}else
d+=f.slice(c,e);if(e==k)break;c=e}a=1;if(++c<k&&((j=f.charCodeAt(c))&-64)==g){i=j+(h<<6);if(h<ao){a=i-12416;if(a<g)a=1}else{a=2;if(++c<k&&((j=f.charCodeAt(c))&-64)==g){i=j+(i<<6);if(h<as){a=i-925824;if(a<at||a>=55295&&a<57344)a=2}else{a=3;if(++c<k&&((j=f.charCodeAt(c))&-64)==g&&h<245){a=j-63447168+(i<<6);if(a<65536||a>1114111)a=3}}}}}if(a<4){c-=a;d+="\ufffd"}else
if(a>65535)d+=String.fromCharCode(55232+(a>>10),ag+(a&1023));else
d+=String.fromCharCode(a);if(d.length>t){d.substr(0,1);l+=d;d=b}}return l+d}function
bu(a){switch(a.t){case
9:return a.c;default:aK(a);case
0:if(aL(a.c)){a.t=9;return a.c}a.t=8;case
8:return bv(a.c)}}function
m(c,a,b){this.t=c;this.c=a;this.l=b}m.prototype.toString=function(){return bu(this)};function
bm(b,a){throw[0,b,a]}function
a(a){return new
m(0,a,a.length)}function
aQ(c,b){bm(c,a(b))}var
d=[0];function
Q(a){aQ(d.Invalid_argument,a)}function
bd(a){if(a<0)Q("Bytes.create");return new
m(a?2:9,b,a)}var
bk=0;function
v(){return bk++}function
be(){var
b=c.console?c.console:{},d=["log","debug","info","warn","error","assert","dir","dirxml","trace","group","groupCollapsed","groupEnd","time","timeEnd"];function
e(){}for(var
a=0;a<d.length;a++)if(!b[d[a]])b[d[a]]=e;return b}var
D={amp:/&/g,lt:/</g,quot:/\"/g,all:/[&<\"]/};function
aM(a){if(!D.all.test(a))return a;return a.replace(D.amp,"&amp;").replace(D.lt,"&lt;").replace(D.quot,"&quot;")}function
bw(f){for(var
h=b,c=h,a,j,d=0,i=f.length;d<i;d++){a=f.charCodeAt(d);if(a<g){for(var
e=d+1;e<i&&(a=f.charCodeAt(e))<g;e++);if(e-d>aF){c.substr(0,1);h+=c;c=b;h+=f.slice(d,e)}else
c+=f.slice(d,e);if(e==i)break;d=e}if(a<at){c+=String.fromCharCode(192|a>>6);c+=String.fromCharCode(g|a&63)}else
if(a<55296||a>=ax)c+=String.fromCharCode(ao|a>>12,g|a>>6&63,g|a&63);else
if(a>=56319||d+1==i||(j=f.charCodeAt(d+1))<ag||j>ax)c+="\xef\xbf\xbd";else{d++;a=(a<<10)+j-56613888;c+=String.fromCharCode(as|a>>18,g|a>>12&63,g|a>>6&63,g|a&63)}if(c.length>t){c.substr(0,1);h+=c;c=b}}return h+c}function
E(a){var
b=9;if(!aL(a))b=8,a=bw(a);return new
m(b,a,a.length)}function
j(a){aQ(d.Sys_error,a)}var
o=new
Array();function
bg(e){var
a=o[e];if(!a.opened)j("Cannot flush a closed channel");if(!a.buffer||a.buffer==b)return 0;if(a.fd&&d.fds[a.fd]&&d.fds[a.fd].output){var
c=d.fds[a.fd].output;switch(c.length){case
2:c(e,a.buffer);break;default:c(a.buffer)}}a.buffer=b;return 0}if(c.process&&c.process.cwd)var
u=c.process.cwd().replace(/\\/g,n);else
var
u="/static";if(u.slice(-1)!==n)u+=n;function
bf(a){a=a
instanceof
m?a.toString():a;if(a.charCodeAt(0)!=47)a=u+a;var
e=a.split(n),c=[];for(var
d=0;d<e.length;d++)switch(e[d]){case"..":if(c.length>1)c.pop();break;case".":break;case"":if(c.length==0)c.push(b);break;default:c.push(e[d]);break}c.orig=a;return c}function
bl(a){a=a
instanceof
m?a.toString():a;j(a+": No such file or directory")}function
bq(a){return new
m(4,a,a.length)}function
aT(){Q("index out of bounds")}function
bs(a,b){switch(a.t&6){default:if(b>=a.c.length)return 0;case
0:return a.c.charCodeAt(b);case
4:return a.c[b]}}function
bp(b,a){if(a>>>0>=b.l)aT();return bs(b,a)}function
r(a){if(a<0)Q("String.create");return new
m(a?2:9,b,a)}function
F(a){return a.l}function
aI(){}function
e(a){this.data=a}e.prototype=new
aI();e.prototype.truncate=function(a){var
b=this.data;this.data=r(a|0);q(b,0,this.data,0,a)};e.prototype.length=function(){return F(this.data)};e.prototype.write=function(b,d,g,a){var
c=this.length();if(b+a>=c){var
e=r(b+a),f=this.data;this.data=e;q(f,0,this.data,0,c)}q(d,g,this.data,b,a);return 0};e.prototype.read=function(c,a,d,b){var
e=this.length();q(this.data,c,a,d,b);return 0};e.prototype.read_one=function(a){return bp(this.data,a)};e.prototype.close=function(){};e.prototype.constructor=e;function
h(b,a){this.content={};this.root=b;this.lookupFun=a}h.prototype.nm=function(a){return this.root+a};h.prototype.lookup=function(b){if(!this.content[b]&&this.lookupFun){var
c=this.lookupFun(a(this.root),a(b));if(c!=0)this.content[b]=new
e(c[1])}};h.prototype.exists=function(a){if(a==b)return 1;var
d=a+n,e=new
RegExp(M+d);for(var
c
in
this.content)if(c.match(e))return 1;this.lookup(a);return this.content[a]?1:0};h.prototype.readdir=function(d){var
g=d==b?b:d+n,h=new
RegExp(M+g+au),e={},c=[];for(var
f
in
this.content){var
a=f.match(h);if(a&&!e[a[1]]){e[a[1]]=true;c.push(a[1])}}return c};h.prototype.is_dir=function(a){var
e=a==b?b:a+n,f=new
RegExp(M+e+au),g=[];for(var
d
in
this.content){var
c=d.match(f);if(c)return 1}return 0};h.prototype.unlink=function(a){var
b=this.content[a]?true:false;delete
this.content[a];return b};h.prototype.open=function(a,b){if(b.rdonly&&b.wronly)j(this.nm(a)+av);if(b.text&&b.binary)j(this.nm(a)+ap);this.lookup(a);if(this.content[a]){if(this.is_dir(a))j(this.nm(a)+" : is a directory");if(b.create&&b.excl)j(this.nm(a)+ar);var
c=this.content[a];if(b.truncate)c.truncate();return c}else
if(b.create){this.content[a]=new
e(r(0));return this.content[a]}else
bl(this.nm(a))};h.prototype.register=function(c,b){if(this.content[c])j(this.nm(c)+ar);if(b
instanceof
m)this.content[c]=new
e(b);else
if(b
instanceof
Array)this.content[c]=new
e(bq(b));else
if(b.toString){var
d=a(b.toString());this.content[c]=new
e(d)}};h.prototype.constructor=h;function
aJ(a){if(a.t!=4)P(a);return a.c}function
bt(a,c,b){b&=255;if(a.t!=4){if(c==a.c.length){a.c+=String.fromCharCode(b);if(c+1==a.l)a.t=0;return 0}P(a)}a.c[c]=b;return 0}function
br(b,a,c){if(a>>>0>=b.l)aT();return bt(b,a,c)}var
O=c.Buffer;function
l(a){this.fs=require("fs");this.fd=a}l.prototype=new
aI();l.prototype.truncate=function(a){this.fs.ftruncateSync(this.fd,a|0)};l.prototype.length=function(){return this.fs.fstatSync(this.fd).size};l.prototype.write=function(g,b,d,f){var
a=aJ(b);if(!(a
instanceof
c.Uint8Array))a=new(c.Uint8Array)(a);var
e=new
O(a);this.fs.writeSync(this.fd,e,d,f,g);return 0};l.prototype.read=function(h,e,d,g){var
a=aJ(e);if(!(a
instanceof
c.Uint8Array))a=new(c.Uint8Array)(a);var
f=new
O(a);this.fs.readSync(this.fd,f,d,g,h);for(var
b=0;b<g;b++)br(e,d+b,f[d+b]);return 0};l.prototype.read_one=function(d){var
b=new(c.Uint8Array)(1),a=new
O(b);this.fs.readSync(this.fd,a,0,1,d);return a[0]};l.prototype.close=function(){this.fs.closeSync(this.fd)};l.prototype.constructor=l;function
k(a){this.fs=require("fs");this.root=a}k.prototype.nm=function(a){return this.root+a};k.prototype.exists=function(a){return this.fs.existsSync(this.nm(a))?1:0};k.prototype.readdir=function(a){return this.fs.readdirSync(this.nm(a))};k.prototype.is_dir=function(a){return this.fs.statSync(this.nm(a)).isDirectory()?1:0};k.prototype.unlink=function(a){var
b=this.fs.existsSync(this.nm(a))?1:0;this.fs.unlinkSync(this.nm(a));return b};k.prototype.open=function(f,c){var
a=require("constants"),b=0;for(var
e
in
c)switch(e){case"rdonly":b|=a.O_RDONLY;break;case"wronly":b|=a.O_WRONLY;break;case"append":b|=a.O_WRONLY|a.O_APPEND;break;case"create":b|=a.O_CREAT;break;case"truncate":b|=a.O_TRUNC;break;case"excl":b|=a.O_EXCL;break;case"binary":b|=a.O_BINARY;break;case"text":b|=a.O_TEXT;break;case"nonblock":b|=a.O_NONBLOCK;break}var
d=this.fs.openSync(this.nm(f),b);return new
l(d)};k.prototype.rename=function(b,a){this.fs.renameSync(this.nm(b),this.nm(a))};k.prototype.constructor=k;var
s=u.match(/[^\/]*\//)[0],w=[];if(typeof
module!=="undefined"&&module.exports&&typeof
require!=="undefined")w.push({path:s,device:new
k(s)});else
w.push({path:s,device:new
h(s)});w.push({path:s+aj,device:new
h(s+aj)});function
bB(b){var
f=bf(b),b=f.join(n),e=b+n,c;for(var
d=0;d<w.length;d++){var
a=w[d];if(e.search(a.path)==0&&(!c||c.path.length<a.path.length))c={path:a.path,device:a.device,rest:b.substring(a.path.length,b.length)}}return c}function
aS(e,f){var
b=o[e],d=a(f),c=F(d);b.file.write(b.offset,d,0,c);b.offset+=c;return 0}function
by(a){var
b=c;if(b.process&&b.process.stdout&&b.process.stdout.write)b.process.stderr.write(a);else{if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
d=b.console;d&&d.error&&d.error(a)}}function
bz(a){var
b=c;if(b.process&&b.process.stdout&&b.process.stdout.write)b.process.stdout.write(a);else{if(a.charCodeAt(a.length-1)==10)a=a.substr(0,a.length-1);var
d=b.console;d&&d.log&&d.log(a)}}function
G(c,f,e,a){if(d.fds===undefined)d.fds=new
Array();a=a?a:{};var
b={};b.file=e;b.offset=a.append?e.length():0;b.flags=a;b.output=f;d.fds[c]=b;if(!d.fd_last_idx||c>d.fd_last_idx)d.fd_last_idx=c;return c}function
bC(c,b,h){var
a={};while(b){switch(b[1]){case
0:a.rdonly=1;break;case
1:a.wronly=1;break;case
2:a.append=1;break;case
3:a.create=1;break;case
4:a.truncate=1;break;case
5:a.excl=1;break;case
6:a.binary=1;break;case
7:a.text=1;break;case
8:a.nonblock=1;break}b=b[2]}if(a.rdonly&&a.wronly)j(c.toString()+av);if(a.text&&a.binary)j(c.toString()+ap);var
e=bB(c),f=e.device.open(e.rest,a),g=d.fd_last_idx?d.fd_last_idx:0;return G(g+1,aS,f,a)}G(0,aS,new
e(r(0)));G(1,bz,new
e(r(0)));G(2,by,new
e(r(0)));function
bh(c){var
b=d.fds[c];if(b.flags.wronly)j(aB+c+" is writeonly");var
a={file:b.file,offset:b.offset,fd:c,opened:true,out:false,refill:null};o[a.fd]=a;return a.fd}function
aN(e){var
c=d.fds[e];if(c.flags.rdonly)j(aB+e+" is readonly");var
a={file:c.file,offset:c.offset,fd:e,opened:true,out:true,buffer:b};o[a.fd]=a;return a.fd}function
bi(){var
b=0;for(var
a=0;a<o.length;a++)if(o[a]&&o[a].opened&&o[a].out)b=[0,o[a].fd,b];return b}function
bj(a){return a
instanceof
Array?a[0]:a
instanceof
m?252:1e3}function
i(c,b,a){d[c+1]=b;if(a)d[a]=b}var
aP={};function
bc(a){if((a.t&6)!=0)aK(a);return a.c}function
bn(a,b){aP[bc(a)]=b;return 0}function
aR(a){return a}function
aO(a){return aP[a]}function
bx(a){if(a
instanceof
Array)return a;if(c.RangeError&&a
instanceof
c.RangeError&&a.message&&a.message.match(/maximum call stack/i))return aR(d.Stack_overflow);if(c.InternalError&&a
instanceof
c.InternalError&&a.message&&a.message.match(/too much recursion/i))return aR(d.Stack_overflow);if(a
instanceof
c.Error&&aO(N))return[0,aO(N),a];return[0,d.Failure,E(String(a))]}function
B(a,b){return a.length==1?a(b):C(a,[b])}var
T=[f,a(ay),-2],H=[f,a(ae),-11];i(11,[f,a(ah),-12],ah);i(10,H,ae);i(9,[f,a(aA),-10],aA);i(8,[f,a(am),-9],am);i(7,[f,a(ak),-8],ak);i(6,[f,a(al),-7],al);i(5,[f,a(az),-6],az);i(4,[f,a(aE),-5],aE);i(3,[f,a(aH),-4],aH);i(2,[f,a(aC),-3],aC);i(1,T,ay);i(0,[f,a(aq),-1],aq);v(0);var
aX=a("Js.Error"),aZ=a("button"),aY=a(ad),ba=a(" !"),bb=a("Hello "),a9=a(an),a_=a("result"),a$=a("b-conv"),a7=a("test1"),a8=a("test3"),a4=a("' textarea"),a5=a(aD),a6=[0,a(af),14,57],a1=a("' element"),a2=a(aD),a3=[0,a(af),9,56];function
p(d,c){var
a=F(d),e=F(c),b=bd(a+e|0);q(d,0,b,0,a);q(c,0,b,a,e);return b}bh(0);aN(1);aN(2);v(0);v(0);var
U=[0,0];function
V(a){U[1]=[0,a,U[1]];return 0}var
x=c,I=null,aW=undefined;function
W(a,b){return a==I?B(b,0):a}var
J=false,X=x.Array,Y=[f,aX,v(0)],K=[0,Y,{}],aV=bj(K)===f?K:K[1];bn(a(N),aV);(function(a){throw a});V(function(a){return a[1]===Y?[0,E(a[2].toString())]:0});V(function(a){return a
instanceof
X?0:[0,E(a.toString())]});function
Z(b,a){b.appendChild(a);return 0}function
L(c){return function(a){if(1-(a==I?1:0)){var
d=B(c,a);if(1-(d|0))a.preventDefault();return d}var
e=event,b=B(c,e);if(1-(b|0))e.returnValue=b;return b}}var
y=x.document;function
z(a,b){return a?B(b,a[1]):0}function
_(b,a){return b.createElement(a.toString())}var
$=[0,ai];function
aa(f,e,d,c){for(;;){if(0===f)if(0===e)return _(d,c);var
h=$[1];if(ai===h){try{var
j=y.createElement('<input name="x">'),k=j.tagName.toLowerCase()===an?1:0,m=k?j.name==="x"?1:0:k,i=m}catch(a){var
i=0}var
l=i?aw:-1003883683;$[1]=l;continue}if(aw<=h){var
a=new
X();a.push("<",c.toString());z(f,function(b){a.push(' type="',aM(b),aG);return 0});z(e,function(b){a.push(' name="',aM(b),aG);return 0});a.push(">");return d.createElement(a.join(b))}var
g=_(d,c);z(f,function(a){return g.type=a});z(e,function(a){return g.name=a});return g}}v(0);x.HTMLElement===aW;var
a0=be(0);function
ab(a){return a0.log(a.toString())}function
A(a){function
b(b){ab(p(a2,p(a,a1)));throw[0,H,a3]}return W(y.getElementById(a.toString()),b)}function
ac(a){var
b=A(a);function
c(b){ab(p(a5,p(a,a4)));throw[0,H,a6]}var
d=b.tagName.toLowerCase()===ad?b:I;return W(d,c)}x.onload=L(function(g){var
e=A(a7),a=aa(0,0,y,aY);a.rows=9;a.cols=20;a.value="Write your matrix";var
b=aa(0,0,y,aZ);b.textContent="Say hello !";b.onclick=L(function(b){a.value="Hello !";return J});Z(e,a);Z(A(a8),b);var
c=ac(a9);c.placeholder="What's your name ?";var
f=ac(a_),d=A(a$);d.textContent="Go";d.onclick=L(function(a){f.value=p(bb,p(E(c.value),ba)).toString();return J});return J});function
aU(b){var
a=b;for(;;){if(a){var
c=a[2],d=a[1];try{bg(d)}catch(a){a=bx(a);if(a[1]!==T)throw a}var
a=c;continue}return 0}}aU(bi(0));return}(function(){return this}()));
