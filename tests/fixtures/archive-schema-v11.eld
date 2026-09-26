;; Literal v11 DDL from 0b93b7612a1742cddf992ed63f1989be5bb3eb74.
("CREATE TABLE IF NOT EXISTS message (
  id           INTEGER PRIMARY KEY,
  stanza_id    TEXT,
  server_id    TEXT,
  account      TEXT NOT NULL,
  peer         TEXT NOT NULL,
  resource     TEXT,
  occupant_id  TEXT,
  direction    TEXT NOT NULL CHECK(direction IN ('in','out')),
  type         TEXT CHECK(type IN ('chat','groupchat','headline')),
  body         TEXT,
  timestamp    INTEGER NOT NULL,
  encrypted    INTEGER DEFAULT 0,
  delivered_at INTEGER,
  displayed_at INTEGER,
  retracted_by TEXT,
  retraction_reason TEXT,
  edited       INTEGER DEFAULT 0,
  reply_to_id  TEXT,
  reply_to_jid TEXT,
  fallback_start INTEGER,
  fallback_end INTEGER,
  thread_id TEXT,
  thread_parent_id TEXT,
  retracted INTEGER NOT NULL DEFAULT 0)"
    "CREATE INDEX IF NOT EXISTS idx_msg_peer_ts
  ON message(account, peer, timestamp)"
    "CREATE INDEX IF NOT EXISTS idx_msg_stanza_id
  ON message(account, stanza_id) WHERE stanza_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_msg_server_id
  ON message(account, server_id) WHERE server_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_msg_occupant_id
  ON message(account, peer, occupant_id) WHERE occupant_id IS NOT NULL"
    "CREATE INDEX IF NOT EXISTS idx_msg_thread
  ON message(account, peer, type, thread_id, timestamp)
  WHERE thread_id IS NOT NULL"
    "CREATE TABLE IF NOT EXISTS message_thread (
  account TEXT NOT NULL,
  peer TEXT NOT NULL,
  type TEXT NOT NULL,
  thread_id TEXT NOT NULL,
  parent_thread_id TEXT,
  root_message_id INTEGER,
  root_stanza_id TEXT,
  root_server_id TEXT,
  created_at INTEGER NOT NULL,
  read_message_id INTEGER,
  dedicated INTEGER NOT NULL DEFAULT 0,
  title TEXT,
  PRIMARY KEY (account, peer, type, thread_id))"
    "CREATE VIRTUAL TABLE IF NOT EXISTS message_fts USING fts5(
  body, content='message', content_rowid='id')"
    "CREATE TRIGGER IF NOT EXISTS message_ai AFTER INSERT ON message BEGIN
  INSERT INTO message_fts(rowid, body) VALUES (new.id, new.body);
END"
    "CREATE TRIGGER IF NOT EXISTS message_ad AFTER DELETE ON message BEGIN
  INSERT INTO message_fts(message_fts, rowid, body)
    VALUES ('delete', old.id, old.body);
END"
    "CREATE TRIGGER IF NOT EXISTS message_au AFTER UPDATE ON message BEGIN
  INSERT INTO message_fts(message_fts, rowid, body)
    VALUES ('delete', old.id, old.body);
  INSERT INTO message_fts(rowid, body) VALUES (new.id, new.body);
END"
    "CREATE TABLE IF NOT EXISTS omemo_store (
  account TEXT PRIMARY KEY,
  store_blob BLOB NOT NULL,
  spk_rotated_at INTEGER)"
    "CREATE TABLE IF NOT EXISTS omemo_sessions (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  session_blob BLOB NOT NULL,
  PRIMARY KEY (account, jid, device_id))"
    "CREATE TABLE IF NOT EXISTS omemo_trust (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  identity_key BLOB NOT NULL,
  trust INTEGER DEFAULT 0,
  first_seen INTEGER NOT NULL,
  PRIMARY KEY (account, jid, device_id))"
    "CREATE TABLE IF NOT EXISTS omemo_skipped_keys (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  dh_key BLOB NOT NULL,
  message_number INTEGER NOT NULL,
  message_key BLOB NOT NULL,
  created_at INTEGER NOT NULL,
  PRIMARY KEY (account, jid, device_id, dh_key, message_number))"
    "CREATE TABLE IF NOT EXISTS omemo_devices (
  account TEXT NOT NULL,
  jid TEXT NOT NULL,
  device_id INTEGER NOT NULL,
  active INTEGER DEFAULT 1,
  last_seen INTEGER NOT NULL,
  PRIMARY KEY (account, jid, device_id))"
    "CREATE INDEX IF NOT EXISTS idx_omemo_trust_jid
  ON omemo_trust (account, jid)"
    "CREATE INDEX IF NOT EXISTS idx_omemo_devices_jid
  ON omemo_devices (account, jid)"
    "CREATE INDEX IF NOT EXISTS idx_omemo_sessions_jid
  ON omemo_sessions (account, jid)"
    "CREATE TABLE IF NOT EXISTS omemo_device_id (
  account TEXT PRIMARY KEY,
  device_id INTEGER NOT NULL)"
    "CREATE TABLE IF NOT EXISTS chat_settings (
  account TEXT NOT NULL,
  peer TEXT NOT NULL,
  encryption TEXT DEFAULT 'default',
  thread_id TEXT,
  PRIMARY KEY (account, peer))"
    "CREATE TABLE IF NOT EXISTS message_oob (
  id         INTEGER PRIMARY KEY,
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  url        TEXT NOT NULL,
  desc       TEXT)"
    "CREATE INDEX IF NOT EXISTS idx_oob_message_id
  ON message_oob(message_id)"
    "CREATE TABLE IF NOT EXISTS message_reaction (
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  sender     TEXT NOT NULL,
  reaction   TEXT NOT NULL,
  updated_at INTEGER NOT NULL,
  PRIMARY KEY (message_id, sender, reaction))"
    "CREATE INDEX IF NOT EXISTS idx_reaction_message_id
  ON message_reaction(message_id)"
    "CREATE TABLE IF NOT EXISTS message_reaction_actor (
  message_id INTEGER NOT NULL REFERENCES message(id) ON DELETE CASCADE,
  sender     TEXT NOT NULL,
  updated_at INTEGER NOT NULL,
  PRIMARY KEY (message_id, sender))"
    "CREATE TABLE IF NOT EXISTS caps_cache (
  hash       TEXT NOT NULL,
  ver        TEXT NOT NULL,
  identities TEXT NOT NULL,
  features   TEXT NOT NULL,
  PRIMARY KEY (hash, ver))")
