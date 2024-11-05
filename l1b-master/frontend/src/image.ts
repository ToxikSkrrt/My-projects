export interface ImageType {
  id: number;
  name: string;
  type: string;
  size: string;
  isgray: boolean;
  likes: number;
}

export interface SimilarType {
  id: number;
  name: string;
  type: string;
  size: string;
  score: number;
  likes: number;
}