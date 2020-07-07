package com.g2forge.reassert.maven;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescriber;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.maven.model.MavenPOM;

import lombok.AccessLevel;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@ToString
@EqualsAndHashCode(callSuper = false)
@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class MavenPOMDescriber implements IDescriber<MavenPOM>, ISingleton {
	@ToString.Exclude
	@EqualsAndHashCode.Exclude
	protected final IContext context;

	@Getter
	protected final ITypeRef<MavenPOM> type = ITypeRef.of(MavenPOM.class);

	@Override
	public IDescription describe(MavenPOM value) {
		final IDescription description = getContext().findSystem(value.getCoordinates()).getCoordinateDescriber().describe(value.getCoordinates());
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return description.getIdentifier() + " pom";
			}

			@Override
			public String getName() {
				return "POM\n" + description.getName();
			}
		};
	}
}