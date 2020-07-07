package com.g2forge.reassert.maven;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;

import lombok.Getter;

public class MavenCoordinatesDescriber implements IDescriber<MavenCoordinates>, ISingleton {
	protected static final MavenCoordinatesDescriber INSTANCE = new MavenCoordinatesDescriber();

	public static MavenCoordinatesDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<MavenCoordinates> type = ITypeRef.of(MavenCoordinates.class);

	protected MavenCoordinatesDescriber() {}

	@Override
	public IDescription describe(MavenCoordinates value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return getName();
			}

			@Override
			public String getName() {
				final StringBuilder retVal = new StringBuilder();
				retVal.append(value.getGroupId()).append(':').append(value.getArtifactId()).append(':').append(value.getVersion());
				if (value.getPackaging() != null) retVal.append(':').append(value.getPackaging().toString().toLowerCase());
				return retVal.toString();
			}
		};
	}
}