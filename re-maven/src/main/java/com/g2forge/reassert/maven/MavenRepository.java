package com.g2forge.reassert.maven;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Collection;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.module.paranamer.ParanamerModule;
import com.g2forge.alexandria.adt.graph.HGraph;
import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.alexandria.service.BasicServiceLoader;
import com.g2forge.alexandria.service.DefaultInstantiator;
import com.g2forge.gearbox.command.process.IProcess;
import com.g2forge.gearbox.maven.HMaven;
import com.g2forge.gearbox.maven.IMaven;
import com.g2forge.gearbox.maven.MavenDownloadErrors;
import com.g2forge.gearbox.maven.MavenPackaging;
import com.g2forge.reassert.cache.CacheAreaDescriptor;
import com.g2forge.reassert.cache.CacheAreaDescriptor.CacheAreaDescriptorBuilder;
import com.g2forge.reassert.cache.store.FileCacheStore;
import com.g2forge.reassert.cache.store.JacksonCacheStore;
import com.g2forge.reassert.core.api.IReassertGraphBuilder;
import com.g2forge.reassert.core.api.parser.IParser;
import com.g2forge.reassert.core.api.system.ARepository;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.artifact.Depends;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.UnspecifiedLicense;
import com.g2forge.reassert.core.model.coordinates.Coordinates;
import com.g2forge.reassert.core.model.file.Describes;
import com.g2forge.reassert.maven.model.MavenDependency;
import com.g2forge.reassert.maven.model.MavenPOM;
import com.g2forge.reassert.maven.model.MavenScope;
import com.g2forge.reassert.maven.modifier.IMavenPOMModifier;
import com.g2forge.reassert.maven.modifier.IMavenPOMModifierFactory;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;
import lombok.extern.slf4j.Slf4j;

@ToString
@EqualsAndHashCode(callSuper = false)
@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
@Slf4j
public class MavenRepository extends ARepository<MavenCoordinates, MavenSystem> {
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(AccessLevel.PUBLIC)
	protected final MavenSystem system;

	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final ObjectMapper jsonMapper = computeJSONMapper();

	/** Cache area for raw POM downloads. */
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction1<? super MavenCoordinates, ? extends Path> pomCacheArea = computePOMCacheArea();

	/** Cache area for resolved (effective) POM files. */
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction1<? super MavenCoordinates, ? extends Path> resolvedCacheArea = computeResolvedCacheArea();

	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IMaven maven = HMaven.getMaven();

	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final Collection<? extends IMavenPOMModifierFactory> factories = computeFactories();

	protected Collection<? extends IMavenPOMModifierFactory> computeFactories() {
		final Collection<? extends IMavenPOMModifierFactory> factories = new BasicServiceLoader<>(null, IMavenPOMModifierFactory.class, null, new DefaultInstantiator<>(null, IMavenPOMModifierFactory.class)).load().toCollection();
		final List<IMavenPOMModifierFactory> sorted = HGraph.<IMavenPOMModifierFactory>toposort(factories, factory -> {
			final Collection<Class<? extends IMavenPOMModifierFactory>> downstream = factory.getDownstream();
			if (downstream == null) return HCollection.emptySet();
			return downstream.stream().flatMap(type -> factories.stream().filter(type::isInstance)).collect(Collectors.toCollection(LinkedHashSet::new));
		}, true);
		return sorted;
	}

	protected ObjectMapper computeJSONMapper() {
		final ObjectMapper jsonMapper = new ObjectMapper();
		jsonMapper.registerModule(new ParanamerModule());
		return jsonMapper;
	}

	protected IFunction1<? super MavenCoordinates, ? extends Path> computePOMCacheArea() {
		final CacheAreaDescriptorBuilder<MavenCoordinates, Path> builder = CacheAreaDescriptor.builder();
		builder.name(Paths.get(getClass().getName()).resolve("pom"));
		builder.function(this::download).hashFunction(c -> hash(c, false));
		builder.keyConverter(new JacksonCacheStore<MavenCoordinates>(getJsonMapper(), ITypeRef.of(MavenCoordinates.class)));
		builder.valueConverter(new FileCacheStore());
		builder.valueName("pom.xml");
		return getSystem().getContext().getCache().createArea(builder.build());
	}

	protected IFunction1<? super MavenCoordinates, ? extends Path> computeResolvedCacheArea() {
		final CacheAreaDescriptorBuilder<MavenCoordinates, Path> builder = CacheAreaDescriptor.builder();
		builder.name(Paths.get(getClass().getName()).resolve("resolved"));
		builder.function(this::resolve).hashFunction(c -> hash(c, true));
		builder.keyConverter(new JacksonCacheStore<MavenCoordinates>(getJsonMapper(), ITypeRef.of(MavenCoordinates.class)));
		builder.valueConverter(new FileCacheStore());
		builder.valueName("pom.xml");
		return getSystem().getContext().getCache().createArea(builder.build());
	}

	protected Path download(MavenCoordinates coordinates, Path path) {
		final IMaven maven = getMaven();
		try {
			final IProcess process = maven.dependencyCopy(path.getParent(), coordinates.toBuilder().packaging(MavenPackaging.POM).build().toMaven(), path.getParent());
			if (HCollection.isOne(MavenDownloadErrors.process(log, process), MavenDownloadErrors.MISSING_ARTIFACT)) {
				Files.newOutputStream(path, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE).close();
				return path;
			}
			process.assertSuccess();

			Files.move(path.getParent().resolve(coordinates.getArtifactId() + "-" + coordinates.getVersion() + ".pom"), path);
		} catch (Throwable throwable) {
			throw new RuntimeException("Failed to cache maven POM!", throwable);
		}

		return path;
	}

	protected Set<ILicenseApplied> getLicenses(final MavenPOM pom) {
		if ((pom.getLicenses() == null) || pom.getLicenses().isEmpty()) return HCollection.asSet(UnspecifiedLicense.create());
		final IParser<ILicenseApplied> licenseParser = getSystem().getContext().getLicenseParser();
		return pom.getLicenses().stream().map(mavenLicense -> licenseParser.parse(mavenLicense.getName())).collect(Collectors.toSet());

	}

	protected Path hash(MavenCoordinates coordinates, boolean modify) {
		try {
			final String groupName = coordinates.getGroupId().replace('.', '/');

			final Path base;
			// If this hash should include modifiers, then look them up, otherwise just use an empty list of modifiers
			final List<IMavenPOMModifier> modifiers = modify ? getFactories().stream().map(modifier -> modifier.getModifier(coordinates)).filter(Objects::nonNull).collect(Collectors.toList()) : HCollection.emptyList();
			if (modifiers.isEmpty()) base = Paths.get(groupName);
			else base = Paths.get(modifiers.stream().map(IMavenPOMModifier::getKey).collect(Collectors.joining("_"))).resolve(groupName);

			return base.resolve(coordinates.getArtifactId()).resolve(coordinates.getVersion());
		} catch (Throwable throwable) {
			throw new RuntimeException(String.format("Failed to hash maven coordinates %1$s", MavenCoordinatesDescriber.create().describe(coordinates).getName()), throwable);
		}
	}

	@Override
	public Artifact<MavenCoordinates> load(MavenCoordinates coordinates, IReassertGraphBuilder builder) {
		final Artifact<MavenCoordinates> artifact = new Artifact<>(this, coordinates);
		builder.vertex(artifact).vertex(coordinates).edge(artifact, coordinates, new Coordinates());

		final MavenCoordinates unpackaged = coordinates.toBuilder().packaging(null).system(null).build();
		final Path pomPath = getPomCacheArea().apply(unpackaged);
		final boolean nonEmpty;
		try {
			nonEmpty = Files.size(pomPath) != 0;
		} catch (IOException e) {
			throw new RuntimeIOException(e);
		}

		if (nonEmpty) {
			final MavenPOM pom = readPom(pomPath);
			builder.vertex(pom).edge(pom, artifact, new Describes());

			final Collection<ILicenseApplied> licenses = getLicenses(pom);
			for (ILicenseApplied license : licenses) {
				builder.vertex(license).edge(artifact, license, new Notice());
			}

			if (pom.getParent() != null) {
				final MavenCoordinates parentCoordinates = validate(pom.getParent().getCoordinates());
				builder.callback(new Artifact<>(this, parentCoordinates), new Inherits(), artifact);
			}

			{
				final Path resolvedPath = getResolvedCacheArea().apply(unpackaged);
				final MavenPOM resolved = readPom(resolvedPath);

				final List<MavenDependency> dependencies = resolved.getDependencies();
				if ((dependencies != null) && !dependencies.isEmpty()) for (MavenDependency dependency : dependencies) {
					final boolean required = !dependency.isOptional();
					final MavenCoordinates dependencyCoordinates = validate(dependency.getCoordinates());
					final Depends depends;
					switch (dependency.getScope()) {
						case Compile:
							depends = new Depends(required, required, true, true);
							break;
						case System:
						case Provided:
							depends = new Depends(false, false, true, true);
							break;
						case Runtime:
							depends = new Depends(required, required, true, false);
							break;
						case Test:
							depends = new Depends(false, false, true, false);
							break;
						case Import:
							depends = null;
							break;
						default:
							throw new EnumException(MavenScope.class, dependency.getScope());
					}
					if (depends != null) builder.callback(new Artifact<>(this, dependencyCoordinates), depends, artifact);
				}
			}
		}

		return artifact;
	}

	protected MavenPOM readPom(final Path path) {
		try (final BufferedReader reader = Files.newBufferedReader(path, Charset.forName("ISO-8859-1"))) {
			return getSystem().getMapper().readValue(reader, MavenPOM.class);
		} catch (IOException exception) {
			throw new RuntimeIOException(String.format("Failed to read maven POM from %1$s", path), exception);
		}
	}

	protected Path resolve(MavenCoordinates coordinates, Path path) {
		final Path raw = getPomCacheArea().apply(coordinates);
		Path current = raw;

		final List<IMavenPOMModifier> modifiers = getFactories().stream().map(modifier -> modifier.getModifier(coordinates)).filter(Objects::nonNull).collect(Collectors.toList());
		for (int i = 0; i < modifiers.size(); i++) {
			final Path next = path.getParent().resolve(i + ".xml");
			modifiers.get(i).accept(current, next);
			if (current != raw) try {
				Files.delete(current);
			} catch (IOException e) {
				throw new RuntimeIOException(e);
			}
			current = next;
		}

		try {
			final Path pom;
			final boolean isCurrentPom = current.getFileName().toString().equals("pom.xml");
			if (isCurrentPom) pom = current;
			else {
				pom = path.getParent().resolve("pom.xml");
				if (current == raw) Files.copy(current, pom);
				else Files.move(current, pom);
			}

			final Path resolved;
			final boolean isSameDirectory = pom.getParent().equals(path.getParent());
			if (isSameDirectory) resolved = path.getParent().resolve("resolved.xml");
			else resolved = path;

			getMaven().effectivePOM(pom.getParent(), resolved).forEach(log::debug);

			if (!isCurrentPom) Files.delete(pom);
			if (isSameDirectory) Files.move(resolved, path);
		} catch (Throwable throwable) {
			throw new RuntimeException(String.format("Failed to generate effective POM for %1$s", getSystem().getCoordinateDescriber().describe(coordinates).getName()), throwable);
		}

		return path;
	}

	protected static MavenCoordinates validate(MavenCoordinates coordinates) {
		if ((coordinates.getGroupId() == null) || (coordinates.getArtifactId() == null) || (coordinates.getVersion() == null)) throw new NullPointerException(MavenCoordinatesDescriber.create().describe(coordinates).getName());
		return coordinates;
	}
}
