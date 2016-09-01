
let map = (shortcuts, command, custom=false) => {
  vimfx.set(`${custom ? 'custom.' : ''}mode.normal.${command}`, shortcuts)
}

//map('e','tab_close');

vimfx.addKeyOverrides(
    [ location => location.hostname === 'www.fastmail.com',
        [ 'd', 'm', '.', 'c', 'g', 'j', 'k', '/', 'x', 'r', 'a', 'u', '"', ':', ';', "'", 'n']
    ]
);

vimfx.set('mode.normal.stop', '<force><C-c>');
vimfx.set('mode.normal.enter_mode_ignore', '<force><C-z>');
