package com.g2forge.reassert.maven.modifier;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import com.g2forge.alexandria.annotations.service.Service;
import com.g2forge.alexandria.java.core.helpers.HBinary;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.io.HBinaryIO;
import com.g2forge.alexandria.java.io.HIO;
import com.g2forge.alexandria.java.io.RuntimeIOException;
import com.g2forge.reassert.maven.MavenCoordinates;
import com.g2forge.reassert.maven.modifier.IMavenPOMModifier;
import com.g2forge.reassert.maven.modifier.IMavenPOMModifierFactory;

@Service(IMavenPOMModifierFactory.class)
public class EntitiesPOMModifierFactory implements IMavenPOMModifierFactory {
	protected static class EntitiesMavenPOMModifier implements IMavenPOMModifier, ISingleton {
		protected static final EntitiesMavenPOMModifier INSTANCE = new EntitiesMavenPOMModifier();

		protected static final String ENTITIES = "https://www.w3.org/2003/entities/2007/w3centities-f.ent";

		public static EntitiesMavenPOMModifier create() {
			return INSTANCE;
		}

		protected EntitiesMavenPOMModifier() {}

		@Override
		public void accept(Path input, Path output) {
			try (final OutputStream stream = Files.newOutputStream(output)) {
				final PrintStream print = new PrintStream(stream);
				print.append("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n<!DOCTYPE project SYSTEM \"").append(ENTITIES).append("\">\n").flush();
				HBinaryIO.copy(Files.newInputStream(input), stream);
			} catch (IOException e) {
				throw new RuntimeIOException(e);
			}
		}

		@Override
		public String getKey() {
			return HBinary.toHex(HIO.sha1(ENTITIES));
		}
	}

	protected static final Set<MavenCoordinates> modify;

	static {
		final Set<MavenCoordinates> set = new HashSet<>();
		set.add(new MavenCoordinates(null, "org.codehaus.plexus", "plexus", "1.0.4", null));
		modify = Collections.unmodifiableSet(set);
	}

	@Override
	public Collection<Class<? extends IMavenPOMModifierFactory>> getDownstream() {
		return HCollection.asList(DistributionManagementStatusPOMModifierFactory.class, MavenPOMModifierFactory.class, PluginSurefirePOMModifierFactory.class);
	}

	@Override
	public IMavenPOMModifier getModifier(MavenCoordinates coordinates) {
		final MavenCoordinates raw = coordinates.toBuilder().system(null).packaging(null).build();
		if (!modify.contains(raw)) return null;

		return EntitiesMavenPOMModifier.create();
	}
}
