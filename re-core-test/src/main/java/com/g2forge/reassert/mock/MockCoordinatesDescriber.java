package com.g2forge.reassert.mock;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.model.coordinates.ICoordinatesDescriber;

import lombok.Getter;

public class MockCoordinatesDescriber implements ICoordinatesDescriber<MockCoordinates>, ISingleton {
	protected static final MockCoordinatesDescriber INSTANCE = new MockCoordinatesDescriber();

	public static MockCoordinatesDescriber create() {
		return INSTANCE;
	}

	@Getter
	protected final ITypeRef<MockCoordinates> type = ITypeRef.of(MockCoordinates.class);

	protected MockCoordinatesDescriber() {}

	@Override
	public IDescription describe(MockCoordinates value) {
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return value.getText();
			}

			@Override
			public String getName() {
				return value.getText();
			}
		};
	}
}