-record(emedia_item, {
    id,
    title,
    class = "object.container",
    type = "container",
    update_id = 0
  }).

-record(emedia_item_item, {
    id,
    parent_id,
    child_id
  }).

-record(emedia_media, {
    id,
    item_id,
    hash,
    type,
    filename,
    fullpath,
    mimetype,
    start_time,
    duration,
    size,
    mtime,
    width,
    height,
    last_scan
  }).
