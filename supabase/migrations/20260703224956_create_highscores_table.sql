create table if not exists public.highscores (
	id bigint generated always as identity primary key,
	name text not null check (char_length(name) between 1 and 12),
	points integer not null check (points >= 0),
	created_at timestamptz not null default now()
);

alter table public.highscores enable row level security;

create policy "public can read highscores"
on public.highscores
for select
to anon
using (true);

create policy "public can insert highscores"
on public.highscores
for insert
to anon
with check (
	char_length(name) between 1 and 12
	and points >= 0
);
