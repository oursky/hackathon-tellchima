defmodule TellchimaWeb.PageController do
  use TellchimaWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
