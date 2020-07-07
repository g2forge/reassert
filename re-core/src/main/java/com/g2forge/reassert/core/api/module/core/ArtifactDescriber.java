package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.system.ISystem;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.coordinates.ICoordinates;

import lombok.Getter;

public class ArtifactDescriber implements IDescriber<Artifact<?>>, ISingleton {
	protected static final ArtifactDescriber INSTANCE = new ArtifactDescriber();

	public static ArtifactDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<Artifact<?>> type = new ATypeRef<Artifact<?>>() {};

	protected ArtifactDescriber() {}

	@Override
	public IDescription describe(Artifact<?> value) {
		return describeTyped(value);
	}

	protected <T extends ICoordinates> IDescription describeTyped(Artifact<T> value) {
		final ISystem<T> system;
		if (value.getRepository() != null) system = value.getRepository().getSystem();
		else {
			@SuppressWarnings("unchecked")
			final ISystem<T> cast = (ISystem<T>) value.getCoordinates().getSystem();
			system = cast;
		}
		final IDescription identified = system.getCoordinateDescriber().describe(value.getCoordinates());

		return new IDescription() {
			@Override
			public String getIdentifier() {
				return identified.getIdentifier() + " " + Artifact.class.getSimpleName().toLowerCase();
			}

			@Override
			public String getName() {
				return identified.getName();
			}
		};
	}

}