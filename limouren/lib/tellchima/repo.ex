defmodule Tellchima.Repo do
  use Ecto.Repo,
    otp_app: :tellchima,
    adapter: Ecto.Adapters.Postgres
end
