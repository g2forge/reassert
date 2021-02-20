package com.g2forge.reassert.git;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.eclipse.jgit.api.Git;
import org.eclipse.jgit.api.errors.GitAPIException;
import org.eclipse.jgit.lib.Constants;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.java.adt.compare.ComparableComparator;
import com.g2forge.alexandria.java.core.helpers.HBinary;
import com.g2forge.alexandria.java.core.helpers.HStream;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.io.HIO;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.io.file.HFile;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.cache.CacheAreaDescriptor;
import com.g2forge.reassert.cache.store.DirectoryCacheStore;
import com.g2forge.reassert.cache.store.ICacheStore;
import com.g2forge.reassert.cache.store.JacksonCacheStore;
import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.api.scanner.IScanner;
import com.g2forge.reassert.core.api.scanner.LocalScannerProxy;
import com.g2forge.reassert.core.api.system.ARepository;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.coordinates.Coordinates;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@ToString
@EqualsAndHashCode(callSuper = false)
@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class GitRepository extends ARepository<GitCoordinates, GitSystem> {
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(AccessLevel.PUBLIC)
	protected final GitSystem system;

	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final ObjectMapper jsonMapper = computeJSONMapper();

	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction1<? super String, ? extends Boolean> serverCacheArea = computeServerCacheArea();

	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction1<? super GitCoordinates, ? extends Path> repositoryCacheArea = computeRepositoryCacheArea();

	protected Path clone(GitCoordinates coordinates, Path path) {
		boolean complete = false;
		try {
			Files.createDirectories(path);
			Git.cloneRepository().setDirectory(path.toFile()).setURI(coordinates.getUrl()).setBranch(coordinates.getBranch()).call().close();
			complete = true;
		} catch (GitAPIException | IOException exception) {
			throw new RuntimeException(exception);
		} finally {
			if (!complete) HFile.delete(path, true);
		}
		return path;
	}

	protected ObjectMapper computeJSONMapper() {
		final ObjectMapper jsonMapper = new ObjectMapper();
		jsonMapper.registerModule(new ParanamerModule());
		return jsonMapper;
	}

	protected IFunction1<? super GitCoordinates, ? extends Path> computeRepositoryCacheArea() {
		final ICacheStore<GitCoordinates> coordinatesStore = new JacksonCacheStore<>(getJsonMapper(), ITypeRef.of(GitCoordinates.class));
		final ICacheStore<Path> repositoryStore = new DirectoryCacheStore() {
			@Override
			public Path load(Path path) {
				final Path retVal = super.load(path);

				final GitConfig config = getSystem().getContext().getConfig().load(ITypeRef.of(GitConfig.class)).or(GitConfig.builder().build());
				if ((config.getAmount() != 0) || (config.getUnit() != null)) {
					final boolean update;
					try {
						final FileTime lastModifiedTime = Files.walk(retVal.resolve(Constants.DOT_GIT).resolve(Constants.LOGS)).map(p -> {
							try {
								return Files.getLastModifiedTime(p);
							} catch (IOException e) {
								throw new RuntimeIOException(e);
							}
						}).max(ComparableComparator.create()).get();
						update = Instant.now().minus(config.getAmount(), config.getUnit()).compareTo(lastModifiedTime.toInstant()) >= 0;
					} catch (IOException e) {
						throw new RuntimeIOException(e);
					}

					if (update) {
						try {
							Git.open(retVal.toFile()).pull().call();
						} catch (GitAPIException | IOException e) {
							throw new RuntimeException(e);
						}
					}
				}

				return retVal;
			}
		};

		final IFunction1<? super GitCoordinates, ? extends Path> hash = c -> Paths.get(HBinary.toHex(HIO.sha1(c.getUrl()))).resolve(c.getBranch() == null ? Constants.HEAD : c.getBranch());
		final CacheAreaDescriptor<GitCoordinates, Path> coordinates = CacheAreaDescriptor.<GitCoordinates, Path>builder().name(Paths.get(getClass().getName())).function(this::clone).hashFunction(hash).keyConverter(coordinatesStore).valueConverter(repositoryStore).build();
		return getSystem().getContext().getCache().createArea(coordinates);
	}

	protected IFunction1<? super String, ? extends Boolean> computeServerCacheArea() {
		final ICacheStore<String> urlStore = new JacksonCacheStore<>(getJsonMapper(), ITypeRef.of(String.class));
		final ICacheStore<Boolean> booleanStore = new JacksonCacheStore<>(getJsonMapper(), ITypeRef.of(Boolean.class));

		final IFunction1<? super String, ? extends Path> hash = c -> Paths.get(HBinary.toHex(HIO.sha1(c)));
		final CacheAreaDescriptor<String, Boolean> coordinates = CacheAreaDescriptor.<String, Boolean>builder().name(Paths.get(getClass().getName())).function(this::getServerInfo).hashFunction(hash).keyConverter(urlStore).valueConverter(booleanStore).build();
		return getSystem().getContext().getCache().createArea(coordinates);
	}

	protected GitCoordinates createCoordinates(GitCoordinates repo, Path root, Path path) {
		final Path subpath = root.relativize(path);

		final String subpathString = HStream.toStream(subpath.iterator()).map(Object::toString).collect(Collectors.joining("/"));
		final String url = repo.getUrl();

		final Matcher matcher = Pattern.compile("((http(s?)://([^/]+))/[^/]+/[^/]+).git").matcher(url);
		if (matcher.matches()) {
			final String serverURL = matcher.group(2);
			if (!getServerCacheArea().apply(serverURL)) throw new IllegalArgumentException(String.format("Cannot generate coordinates for files in %1$s, because %2$s does not appear to be a github server", url, serverURL));
			return new GitCoordinates(repo.getSystem(), matcher.group(1) + "/blob/master/" + subpathString, repo.getBranch());
		}

		throw new IllegalArgumentException(String.format("Cannot generate coordinates for files in %1$s", url));
	}

	protected boolean getServerInfo(String url, Path path) {
		final Pattern REGEX = Pattern.compile("(https?)://([^/]+)(/.*)?");
		final Matcher matcher = REGEX.matcher(url);
		if (!matcher.matches()) throw new IllegalArgumentException(url);
		final String urlProtocol = matcher.group(1);
		final String urlDNS = matcher.group(2);

		if (urlDNS.matches("([^.]+\\.)?github\\.com")) return true;
		else {
			try {
				final HttpURLConnection connection = (HttpURLConnection) new URL(urlProtocol + "://" + urlDNS + "/api/v3/").openConnection();
				final int timeout = 5000;
				connection.setConnectTimeout(timeout);
				connection.setReadTimeout(timeout);
				connection.setInstanceFollowRedirects(true);

				final int status = connection.getResponseCode();
				if (status != 200) return false;
				return connection.getHeaderFields().keySet().contains("X-GitHub-Enterprise-Version");
			} catch (Throwable throwable) {
				throw new RuntimeException(throwable);
			}
		}
	}

	@Override
	public Artifact<GitCoordinates> load(GitCoordinates coordinates, IReassertGraphBuilder builder) {
		assertValid(coordinates);
		final Artifact<GitCoordinates> artifact = new Artifact<>(this, coordinates);
		builder.vertex(artifact).vertex(coordinates).edge(artifact, coordinates, new Coordinates());

		final IScanner scanner = getSystem().getContext().getScanner();
		if (scanner != null) {
			final Path root = getRepositoryCacheArea().apply(coordinates);
			new LocalScannerProxy(scanner, p -> createCoordinates(coordinates, root, p)).scan(root, builder, artifact);
		}

		return artifact;
	}
}
