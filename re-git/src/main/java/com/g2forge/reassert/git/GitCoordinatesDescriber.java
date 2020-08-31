package com.g2forge.reassert.git;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.coordinates.ICoordinatesDescriber;

import lombok.Getter;

public class GitCoordinatesDescriber implements ICoordinatesDescriber<GitCoordinates>, ISingleton {
	protected static final GitCoordinatesDescriber INSTANCE = new GitCoordinatesDescriber();

	public static GitCoordinatesDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<GitCoordinates> type = ITypeRef.of(GitCoordinates.class);

	protected GitCoordinatesDescriber() {}

	@Override
	public IDescription describe(GitCoordinates value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return getName();
			}

			@Override
			public String getName() {
				return value.getUrl() + ((value.getBranch() == null) ? "" : " @ " + value.getBranch());
			}
		};
	}
}