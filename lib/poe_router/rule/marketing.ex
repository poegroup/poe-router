defmodule PoeRouter.Rule.Marketing do
  defstruct backend: nil,
            backend_host: nil
end

defimpl PoeRouter.Config.Nginx, for: PoeRouter.Rule.Marketing do
  alias PoeRouter.Utils

  def to_string(%{backend: nil}, _), do: ""
  def to_string(%{backend_host: nil}, _), do: ""
  def to_string(%{backend: backend, backend_host: backend_host}, _) do
    uri = URI.parse(backend)
    zone = "zone_" <> Base.encode16(:crypto.hash(:sha, "#{backend}::::#{backend_host}"))

    ## TODO implement caching

    """
    location / {
      if ($http_x_forwarded_proto != 'https') { return 301 https://$server_name$request_uri; }
      try_files /not_found @marketing_backend;
    }
    location @marketing_backend {
      rewrite                 /?(.*)  #{Utils.join(uri.path, "$1")} break;
      proxy_pass              http://#{uri.host};
      proxy_redirect          off;
      proxy_intercept_errors  on;
      proxy_set_header        host #{backend_host || uri.host};
      # cache
      # proxy_cache             #{zone};
      # proxy_cache_lock on;
      # proxy_cache_use_stale updating;
      error_page 301 =200 @marketing_redirect;
    }
    location @marketing_redirect {
      set $redirect_location $upstream_http_location;
      if ($redirect_location ~ /[^\/]$/) { return 301 $redirect_location; }
      proxy_pass $redirect_location;
      # cache
      # proxy_cache             #{zone};
      # proxy_cache_lock on;
      # proxy_cache_use_stale updating;
    }
    """
  end
end
